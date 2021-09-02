(keymap :n "]g" "<Cmd>lua vim.lsp.diagnostic.goto_next({enable_popup=false})<CR>")
(keymap :n "[g" "<Cmd>lua vim.lsp.diagnostic.goto_prev({enable_popup=false})<CR>")

(local ns (vim.api.nvim_create_namespace :diagnostics))
(fn print-diagnostics []
  (let [diagnostics (vim.lsp.diagnostic.get_line_diagnostics)]
    (if (empty-or-nil? diagnostics)
        (vim.api.nvim_buf_clear_namespace 0 ns 0 -1)
        (let [[line _] (vim.api.nvim_win_get_cursor 0)
              line (- line 1)
              chunks (vim.lsp.diagnostic.get_virtual_text_chunks_for_line 0 line diagnostics)]
          (vim.api.nvim_buf_set_extmark 0 ns line 0 {:id 1 :virt_text chunks :hl_mode :combine})))))

(-> vim.lsp.diagnostic.on_publish_diagnostics
   (vim.lsp.with {:virtual_text false :underline false :severity_sort true})
   (->> (tset vim.lsp.handlers "textDocument/publishDiagnostics")))

(augroup lspconfig#
  (autocmd :BufWinEnter "*"
    (autocmd lspconfig# :BufWritePost "<buffer>" :once
      (vim.lsp.diagnostic.enable)
      (autocmd lspconfig# [:CursorHold :CursorMoved] "<buffer>" (print-diagnostics))))
  (autocmd :User :LspDiagnosticsChanged
    (vim.lsp.diagnostic.set_loclist {:open false}))
  (autocmd :FileType "go,c,cpp,rust,python" :once
    (exec "packadd nvim-lspconfig")
    (with-module [lspconfig :lspconfig]
      ; Fallback to tags if LSP fails to find a definition
      (let [handler (. vim.lsp.handlers "textDocument/definition")]
        (tset vim.lsp.handlers "textDocument/definition"
              (fn [_ method result]
                (if (not (empty-or-nil? result))
                    (handler _ method result)
                    (-> (vim.api.nvim_replace_termcodes "<C-]>" true true true)
                        (vim.api.nvim_feedkeys "n" true))))))

      (fn on-attach [client bufnr]
        (vim.api.nvim_buf_set_option bufnr :omnifunc "v:lua.vim.lsp.omnifunc")

        (keymap :n "<C-]>" "<Cmd>lua vim.lsp.buf.definition()<CR>" {:buffer bufnr})
        (keymap :n "gr" "<Cmd>lua vim.lsp.buf.references()<CR>" {:buffer bufnr})
        (keymap :n "gR" "<Cmd>lua vim.lsp.buf.rename()<CR>" {:buffer bufnr})

        ; Disable when attached, re-enable on BufWrite
        (vim.lsp.diagnostic.disable bufnr)

        (with-module [lsp-compl :lsp_compl]
          (vim.opt.completeopt:append [:noselect :noinsert])
          (lsp-compl.attach client bufnr {:server_side_fuzzy_completion true :trigger_on_delete true}))

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
              (config.autostart))))))))
