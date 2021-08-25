(local servers {:rust_analyzer {}
                :clangd {}
                :gopls {:analyses {:unusedparams true :unusedwrite true :nilness true}}
                :pyright {}})

(fn show-cursor-virtual-text [ns bufnr]
  (let [[line] (vim.api.nvim_win_get_cursor 0)
        line (- line 1)
        id 1
        {: mode} (vim.api.nvim_get_mode)]
    (when (= mode :n)
      (let [items (vim.lsp.diagnostic.get_line_diagnostics bufnr line)]
        (if (empty-or-nil? items)
            (vim.api.nvim_buf_del_extmark bufnr ns id)
            (let [chunks (vim.lsp.diagnostic.get_virtual_text_chunks_for_line bufnr line items)]
              (vim.api.nvim_buf_set_extmark bufnr ns line 0 {: id :virt_text chunks})))))))

; Fallback to tags if LSP fails to find a definition
(let [handler (. vim.lsp.handlers "textDocument/definition")]
  (tset vim.lsp.handlers "textDocument/definition"
        (fn [_ method result]
          (if (not (empty-or-nil? result))
              (handler _ method result)
              (-> (vim.api.nvim_replace_termcodes "<C-]>" true true true)
                  (vim.api.nvim_feedkeys "n" true))))))

(autocmd :my-lspconfig :FileType "go,c,cpp,rust,python" :once
  (exec "packadd nvim-lspconfig")
  (with-module [lspconfig :lspconfig]
    (local ns (vim.api.nvim_create_namespace :diagnostics))
    (fn on-attach [client bufnr]
      (vim.api.nvim_buf_set_option bufnr :omnifunc "v:lua.vim.lsp.omnifunc")

      (keymap :n "<C-]>" "<Cmd>lua vim.lsp.buf.definition()<CR>" {:buffer bufnr})
      (keymap :n "gr" "<Cmd>lua vim.lsp.buf.references()<CR>" {:buffer bufnr})
      (keymap :n "gR" "<Cmd>lua vim.lsp.buf.rename()<CR>" {:buffer bufnr})

      (-> vim.lsp.diagnostic.on_publish_diagnostics
          (vim.lsp.with {:virtual_text false :underline false})
          (->> (tset vim.lsp.handlers "textDocument/publishDiagnostics")))

      (vim.lsp.diagnostic.disable bufnr)

      (autocmd :my-lspconfig :BufWritePost (: "<buffer=%d>" :format bufnr)
        (vim.lsp.diagnostic.enable bufnr)
        (augroup :my-lspconfig
          (autocmd :CursorMoved (: "<buffer=%d>" :format bufnr)
            (show-cursor-virtual-text ns bufnr))
          (autocmd :User :LspDiagnosticsChanged
            (vim.lsp.diagnostic.set_loclist {:open false})
            (show-cursor-virtual-text ns bufnr))))

      (with-module [lsp-compl (require "lsp_compl")]
        (lsp-compl.attach client bufnr {:server_side_fuzzy_completion true :trigger_on_delete true}))

      (exec "doautocmd User LspAttached"))

    (each [name settings (pairs servers)]
      (let [{name config} lspconfig]
        (config.setup {: on_attach
                       :settings {name settings}
                       :flags {:debounce_text_changes 150}})

        (when (vim.tbl_contains config.filetypes vim.bo.filetype)
          (config.autostart))))))
