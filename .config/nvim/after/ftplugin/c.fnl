(local {: set-path} (require "ft/c"))

(set-path (nvim.get_current_buf))

(set vim.bo.commentstring "//%s")

(fn switch-source-header []
  (let [bufnr (nvim.get_current_buf)
        uri (vim.uri_from_bufnr bufnr)
        [client] (vim.lsp.get_active_clients {: bufnr :name :clangd})]
    (when client
      (client.request
        :textDocument/switchSourceHeader
        {: uri}
        (fn [err res]
          (when err
            (error err))
          (if res
              (nvim.command (.. "edit " (vim.uri_to_fname res)))
              (echo "Alternate file not found")))
        bufnr))))

(keymap :n "<Space>cf" switch-source-header {:buffer true})
