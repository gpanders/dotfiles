(local {: set-path} (require "ft/c"))

(set-path (nvim.get_current_buf))

(autocmd ft/c :LspAttach {:buffer 0}
  #(do
    (let [bufnr (nvim.get_current_buf)
          uri (vim.uri_from_bufnr bufnr)
          [client] (vim.lsp.get_clients {: bufnr :name :clangd})]
      (when client
        (fn switch-source-header []
          (client:request
            :textDocument/switchSourceHeader
            {: uri}
            (fn [err res]
              (when err
                (error err))
              (if res
                  (nvim.command (.. "edit " (vim.uri_to_fname res)))
                  (echo "Alternate file not found")))
            bufnr))

        (keymap :n "gh" switch-source-header {:buffer true})

        (let [name (vim.api.nvim_buf_get_name bufnr)]
          (case (vim.fs.find ".clang-format" {:path (vim.fs.dirname name)
                                              :upward true
                                              :type :file
                                              :stop (vim.fs.dirname client.root_dir)})
            [f] (tset vim.b bufnr :lsp_autoformat true)))))))
