(autocmd lsp :FileType "go,c,cpp,rust,python" :once
  (exec "packadd nvim-lspconfig")
  (with-module [lspconfig :lspconfig]
    ; Fallback to tags if LSP fails to find a definition
    (let [handler (. vim.lsp.handlers "textDocument/definition")]
      (tset vim.lsp.handlers "textDocument/definition"
            (fn [_ result ...]
              (if (not (empty-or-nil? result))
                  (handler _ result ...)
                  (-> (vim.api.nvim_replace_termcodes "<C-]>" true true true)
                      (vim.api.nvim_feedkeys "n" true))))))

    (fn on-attach [client bufnr]
      (vim.api.nvim_buf_set_option bufnr :omnifunc "v:lua.vim.lsp.omnifunc")

      (keymap :n "<C-]>" "<Cmd>lua vim.lsp.buf.definition()<CR>" {:buffer bufnr})
      (keymap :n "gr" "<Cmd>lua vim.lsp.buf.references()<CR>" {:buffer bufnr})
      (keymap :n "gR" "<Cmd>lua vim.lsp.buf.rename()<CR>" {:buffer bufnr})

      (with-module [ts-config :nvim-treesitter.configs]
        (ts-config.detach_module :refactor.highlight_definitions bufnr))

      (when (?. client :capabilities :textDocument :documentHighlight)
        (augroup lsp
          (autocmd :CursorHold "<buffer>" (vim.lsp.buf.document_highlight))
          (autocmd [:InsertEnter :CursorMoved] "<buffer>" (vim.lsp.buf.clear_references))))

      (with-module [lsp-compl :lsp_compl]
        (vim.opt.completeopt:append [:noinsert])
        (lsp-compl.attach client bufnr {:server_side_fuzzy_completion true
                                        :trigger_on_delete true}))

      (exec "doautocmd User LspAttached"))

    (let [servers {:rust_analyzer {}
                   :clangd {}
                   :gopls {:analyses {:unusedparams true :unusedwrite true :nilness true}}
                   :pyright {}}]
      (each [name settings (pairs servers)]
        (let [{name config} lspconfig]
          (config.setup {:on_attach on-attach
                         :settings {name settings}
                         :flags {:debounce_text_changes 150}})

          (when (vim.tbl_contains config.filetypes vim.bo.filetype)
            (config.autostart)))))))
