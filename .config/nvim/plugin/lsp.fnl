(fn on-init [client result]
  (when (client:supports_method :textDocument/signatureHelp)
    (set client.server_capabilities.signatureHelpProvider.triggerCharacters []))

  ; Handle off-spec "offsetEncoding" server capability
  (match result.offsetEncoding
    enc (set client.offset_encoding enc)))

(augroup lsp#
  (autocmd :LspAttach
    (fn [{: buf :data {: client_id}}]
      (local client (vim.lsp.get_client_by_id client_id))
      (tset vim.b buf :lsp client.name)
      (when (client:supports_method :textDocument/documentHighlight)
        (augroup lsp#
          (autocmd [:CursorHold :InsertLeave] {:buffer buf} vim.lsp.buf.document_highlight)
          (autocmd [:CursorMoved :InsertEnter] {:buffer buf} vim.lsp.buf.clear_references)))
      (when (client:supports_method :textDocument/inlayHint)
        (keymap :n
                "yoh"
                #(vim.lsp.inlay_hint.enable (not (vim.lsp.inlay_hint.is_enabled {:bufnr buf})) {:bufnr buf})
                {:buffer buf :desc "Toggle inlay hints"}))
      (when (client:supports_method :textDocument/codeLens)
        (autocmd lsp# :LspProgress :end
          (fn [args]
            (when (= args.buf buf)
              (vim.lsp.codelens.refresh {:bufnr buf}))))
        (autocmd lsp# [:BufEnter :TextChanged :InsertLeave] {:buffer buf} #(vim.lsp.codelens.refresh {:bufnr buf}))
        (vim.lsp.codelens.refresh {:bufnr buf}))

      ; Remove these and use defaults when there's a better way to set the border
      (when (client:supports_method :textDocument/hover)
        (keymap :n :K #(vim.lsp.buf.hover {:border :rounded}) {:buffer buf}))
      (when (client:supports_method :textDocument/signatureHelp)
        (keymap :i :<C-S> #(vim.lsp.buf.signature_help {:border :rounded :focusable false})))

      (when (client:supports_method :textDocument/foldingRange)
        (tset vim.wo 0 0 :foldmethod :expr)
        (tset vim.wo 0 0 :foldexpr "v:lua.vim.lsp.foldexpr()"))

      (when (client:supports_method :textDocument/formatting)
        (autocmd lsp# :BufWritePre {:buffer buf}
          #(when (vim.F.if_nil client.settings.autoformat (?. vim.b.lsp :autoformat) (?. vim.g.lsp :autoformat) false)
             (vim.lsp.buf.format {:bufnr buf :id client_id}))))

      (when (client:supports_method :textDocument/completion)
        (match client.name
          :lua-language-server (set client.server_capabilities.completionProvider.triggerCharacters ["." ":"]))
        (vim.lsp.completion.enable true client_id buf {:autotrigger true}))))
  (autocmd :LspDetach
    (fn [{: buf :data {: client_id}}]
      (tset vim.b buf :lsp nil)
      (autocmd! lsp# "*" {:buffer buf})))
  (autocmd :LspProgress "*" "redrawstatus"))

(fn enable []
  (when (not vim.g.lsp)
    (set vim.g.lsp {}))
  (set vim.g.lsp.autostart true)
  (exec "doautoall <nomodeline> FileType"))

(fn disable []
  (when (not vim.g.lsp)
    (set vim.g.lsp {}))
  (set vim.g.lsp.autostart false)
  (vim.lsp.stop_client (vim.lsp.get_clients)))

(command :LspStart {} enable)
(command :LspStop {} disable)

(vim.lsp.config "*" {:on_init on-init})
(let [configs (collect [_ c (ipairs (vim.api.nvim_get_runtime_file "lsp/*.lua" true))]
                (values (vim.fn.fnamemodify c ":t:r") true))]
  (vim.lsp.enable (icollect [c (pairs configs)] c) (or (?. vim.g.lsp :autostart) true)))
