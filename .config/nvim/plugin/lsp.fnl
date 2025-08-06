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
      (tset vim.b buf :lsp_client client.name)
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

      (when (client:supports_method :textDocument/foldingRange)
        (tset vim.wo 0 0 :foldmethod :expr)
        (tset vim.wo 0 0 :foldexpr "v:lua.vim.lsp.foldexpr()"))

      (when (and (not (client:supports_method :textDocument/willSaveWaitUntil))
                 (client:supports_method :textDocument/formatting))
        (autocmd lsp# :BufWritePre {:buffer buf}
          #(when (vim.F.if_nil client.settings.autoformat vim.b.lsp_autoformat vim.g.lsp_autoformat false)
             (vim.lsp.buf.format {:bufnr buf :id client_id}))))

      (when (client:supports_method :textDocument/completion)
        (match client.name
          :lua-language-server (set client.server_capabilities.completionProvider.triggerCharacters ["." ":"]))
        (vim.lsp.completion.enable true client_id buf {:autotrigger true}))

      (when (client:supports_method :textDocument/documentColor)
        (vim.lsp.document_color.enable true buf {:style :virtual}))))
  (autocmd :LspDetach
    (fn [{: buf :data {: client_id}}]
      (tset vim.b buf :lsp nil)
      (autocmd! lsp# "*" {:buffer buf})))
  (autocmd :LspProgress
    (fn [{: buf :data {: client_id :params {: value}}}]
      (case value.kind
        :begin (io.stdout:write "\027]9;4;1;0\027\\")
        :end (io.stdout:write "\027]9;4;0;0\027\\")
        :report (when (and value.percentage (<= 0 value.percentage 100))
                  (io.stdout:write (: "\027]9;4;1;%d\027\\" :format value.percentage))))
      nil)))

(fn enable []
  (set vim.g.lsp_autostart true)
  (exec "doautoall <nomodeline> FileType"))

(fn disable []
  (set vim.g.lsp_autostart false)
  (vim.lsp.stop_client (vim.lsp.get_clients)))

(command :LspStart {} enable)
(command :LspStop {} disable)

(vim.lsp.config "*" {:on_init on-init :workspace_required true})
(let [configs (collect [_ c (ipairs (vim.api.nvim_get_runtime_file "lsp/*.lua" true))]
                (values (vim.fn.fnamemodify c ":t:r") true))]
  (vim.lsp.enable (icollect [c (pairs configs)] c) (or vim.g.lsp_autostart true)))
