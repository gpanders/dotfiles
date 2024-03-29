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
                #(vim.lsp.inlay_hint.enable buf (not (vim.lsp.inlay_hint.is_enabled)))
                {:buffer buf :desc "Toggle inlay hints"}))
      (when (client.supports_method :textDocument/codeLens)
        (autocmd lsp# :LspProgress :end
          (fn [args]
            (when (= args.buf buf)
              (vim.lsp.codelens.refresh))))
        (autocmd lsp# [:BufEnter :TextChanged :InsertLeave] {:buffer buf} vim.lsp.codelens.refresh)
        (vim.lsp.codelens.refresh))
      (keymap :n "gr" vim.lsp.buf.references {:buffer buf})
      (keymap :i "<C-S>" vim.lsp.buf.signature_help {:buffer buf})
      (keymap :n "crr" vim.lsp.buf.rename {:buffer buf})
      (keymap :n "<M-CR>" vim.lsp.buf.code_action {:buffer buf})

      (when (client.supports_method :textDocument/formatting)
        (autocmd lsp# :BufWritePre {:buffer buf}
          #(when (vim.F.if_nil (?. vim.b.lsp :autoformat) vim.g.lsp.autoformat false)
             (vim.lsp.buf.format {:bufnr buf :id client_id}))))

      (when (client.supports_method :textDocument/completion)
        (with-module [compl :lsp_compl]
          (match client.name
            :lua-language-server (set client.server_capabilities.completionProvider.triggerCharacters ["." ":"]))
          (exec "set completeopt+=noinsert")
          (let [{: expand_snippet} (require :snippy)]
            (set compl.expand_snippet expand_snippet))
          (keymap :i :<CR> #(if (compl.accept_pum) :<C-Y> :<CR>) {:expr true :buffer true})
          (keymap :i :<C-Space> #(compl.trigger_completion) {:buffer true})
          (compl.attach client buf {})))))
  (autocmd :LspDetach
    (fn [{: buf :data {: client_id}}]
      (tset vim.b buf :lsp nil)
      (with-module [compl :lsp_compl]
        (compl.detach client_id buf))
      (autocmd! lsp# "*" {:buffer buf})))
  (autocmd :LspProgress "*" "redrawstatus"))

(command :LspStart {} #(let [lsp (require :lsp)] (lsp.enable)))
(command :LspStop {} #(let [lsp (require :lsp)] (lsp.disable)))
