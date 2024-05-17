(fn before-init [params config]
  ; Add utf-8 to positionEncodings capability. Put it in the first position so
  ; that it has highest priority
  (table.insert params.capabilities.general.positionEncodings 1 :utf-8)

  ; Add lsp-compl capabilities if it's installed
  (with-module [compl :lsp_compl]
    (set params.capabilities (vim.tbl_deep_extend :force params.capabilities (compl.capabilities)))))

(fn on-init [client result]
  ; Configure lsp-compl
  (with-module [compl :lsp_compl]
    (set vim.lsp.text_document_completion_list_to_complete_items compl.text_document_completion_list_to_complete_items)
    (when (client.supports_method :textDocument/signatureHelp)
      (set client.server_capabilities.signatureHelpProvider.triggerCharacters [])))

  ; Handle off-spec "offsetEncoding" server capability
  (match result.offsetEncoding
    enc (set client.offset_encoding enc))

  ; Modify default handlers
  (tset client.handlers :textDocument/hover (vim.lsp.with vim.lsp.handlers.hover {:border :rounded}))
  (tset client.handlers :textDocument/signatureHelp (vim.lsp.with vim.lsp.handlers.signature_help {:border :rounded
                                                                                                   :focusable false})))

(augroup lsp#
  (autocmd :LspAttach
    (fn [{: buf :data {: client_id}}]
      (local client (vim.lsp.get_client_by_id client_id))
      (tset vim.b buf :lsp client.name)
      (when (client.supports_method :textDocument/documentHighlight)
        (augroup lsp#
          (autocmd [:CursorHold :InsertLeave] {:buffer buf} vim.lsp.buf.document_highlight)
          (autocmd [:CursorMoved :InsertEnter] {:buffer buf} vim.lsp.buf.clear_references)))
      (when (client.supports_method :textDocument/inlayHint)
        (keymap :n
                "yoh"
                #(vim.lsp.inlay_hint.enable (not (vim.lsp.inlay_hint.is_enabled {:bufnr buf})) {:bufnr buf})
                {:buffer buf :desc "Toggle inlay hints"}))
      (when (client.supports_method :textDocument/codeLens)
        (autocmd lsp# :LspProgress :end
          (fn [args]
            (when (= args.buf buf)
              (vim.lsp.codelens.refresh {:bufnr buf}))))
        (autocmd lsp# [:BufEnter :TextChanged :InsertLeave] {:buffer buf} #(vim.lsp.codelens.refresh {:bufnr buf}))
        (vim.lsp.codelens.refresh {:bufnr buf}))

      (when (client.supports_method :textDocument/formatting)
        (autocmd lsp# :BufWritePre {:buffer buf}
          #(when (vim.F.if_nil (?. vim.b.lsp :autoformat) (?. vim.g.lsp :autoformat) false)
             (vim.lsp.buf.format {:bufnr buf :id client_id}))))

      (when (and compl (client.supports_method :textDocument/completion))
        (match client.name
          :lua-language-server (set client.server_capabilities.completionProvider.triggerCharacters ["." ":"]))
        (exec "set completeopt+=noinsert")
        (let [{: expand_snippet} (require :snippy)]
          (set compl.expand_snippet expand_snippet))
        (keymap :i :<CR> #(if (compl.accept_pum) :<C-Y> :<CR>) {:expr true :buffer true})
        (keymap :i :<C-Space> #(compl.trigger_completion) {:buffer true})
        (compl.attach client buf {}))))
  (autocmd :LspDetach
    (fn [{: buf :data {: client_id}}]
      (tset vim.b buf :lsp nil)
      (when compl
        (compl.detach client_id buf))
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

(let [lsp (require :lsp)]
  (lsp.config [:clangd :gopls :lua-language-server :rust-analyzer :zls :pyright]
              {:autostart (?. vim.g.lsp :autostart)
               :before_init before-init
               :on_init on-init}))
