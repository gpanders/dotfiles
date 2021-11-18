(autocmd lsp :FileType "go,c,cpp,rust,python,zig" :once
  (exec "packadd nvim-lspconfig")
  (with-module [lspconfig :lspconfig]
    (fn on-attach [client bufnr]
      (tset vim.bo bufnr :omnifunc "v:lua.vim.lsp.omnifunc")
      (tset vim.bo bufnr :tagfunc "v:lua.vim.lsp.tagfunc")

      (keymap :n "gr" "<Cmd>lua vim.lsp.buf.references()<CR>" {:buffer bufnr})
      (keymap :n "gR" "<Cmd>lua vim.lsp.buf.rename()<CR>" {:buffer bufnr})
      (keymap :n "<Bslash>c" "<Cmd>lua vim.lsp.buf.code_action()<CR>" {:buffer bufnr})

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
                   :pyright {}
                   :zls {}}]
      (each [name settings (pairs servers)]
        (let [{name config} lspconfig]
          (config.setup {:on_attach on-attach
                         :settings {name settings}
                         :flags {:debounce_text_changes 150}})

          (when (vim.tbl_contains config.filetypes vim.bo.filetype)
            (config.manager.try_add)))))))
