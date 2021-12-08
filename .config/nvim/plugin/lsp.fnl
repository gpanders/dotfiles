(autocmd lsp :FileType "go,c,cpp,rust,python,zig,lua" :once
  (exec "packadd nvim-lspconfig")
  (with-module [lspconfig :lspconfig]
    (fn on-attach [client bufnr]
      (when client.resolved_capabilities.completion
        (tset vim.bo bufnr :omnifunc "v:lua.vim.lsp.omnifunc"))
      (when client.resolved_capabilities.goto_definition
        (tset vim.bo bufnr :tagfunc "v:lua.vim.lsp.tagfunc"))
      (when client.resolved_capabilities.hover
        (tset vim.bo bufnr :keywordprg ":Lsp hover"))
      (when client.resolved_capabilities.document_highlight
        (with-module [ts-config :nvim-treesitter.configs]
          (ts-config.detach_module :refactor.highlight_definitions bufnr))
        (augroup lsp
          (autocmd :CursorHold "<buffer>" (vim.lsp.buf.document_highlight))
          (autocmd [:InsertEnter :CursorMoved] "<buffer>" (vim.lsp.buf.clear_references))))
      (when client.resolved_capabilities.signature_help
        (keymap :i "<C-S>" "<Cmd>lua vim.lsp.buf.signature_help()<CR>" {:buffer bufnr}))
      (when client.resolved_capabilities.find_references
        (keymap :n "<Bslash>r" "<Cmd>lua vim.lsp.buf.references()<CR>" {:buffer bufnr}))

      (with-module [lsp-compl :lsp_compl]
        (vim.opt.completeopt:append [:noinsert])
        (lsp-compl.attach client bufnr {:server_side_fuzzy_completion true
                                        :trigger_on_delete true}))

      (command :Lsp {:nargs "*" :buffer true :complete "custom,lsp#complete"}
               (fn [bang mods args]
                 (let [[cmd & args] (vim.split args " ")]
                   (match cmd
                     :code_action (vim.lsp.buf.code_action)
                     :hover (vim.lsp.buf.hover)
                     :definition (vim.lsp.buf.definition)
                     :formatting (vim.lsp.buf.formatting)
                     :references (vim.lsp.buf.references)
                     :rename (vim.lsp.buf.rename (. args 1))
                     :signature_help (vim.lsp.buf.signature_help)))))

      (exec "doautocmd User LspAttached"))

    (let [servers {:rust_analyzer {}
                   :clangd {}
                   :gopls {:settings {:analyses {:unusedparams true :unusedwrite true :nilness true}}}
                   :pyright {}
                   :zls {}
                   :sumneko_lua {:cmd [:lua-language-server]}}]
      (each [name {: cmd : settings} (pairs servers)]
        (let [{name config} lspconfig]
          (config.setup {:on_attach on-attach
                         : cmd
                         :settings {name settings}
                         :flags {:debounce_text_changes 150}})

          (when (vim.tbl_contains config.filetypes vim.bo.filetype)
            (config.manager.try_add)))))))
