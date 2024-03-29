(local {: set-path} (require "ft/c"))

(set-path (nvim.get_current_buf))

(set vim.bo.commentstring "//%s")

(autocmd ft/c :LspAttach "<buffer>"
  #(do
    (fn switch-source-header []
      (let [bufnr (nvim.get_current_buf)
            uri (vim.uri_from_bufnr bufnr)
            [client] (vim.lsp.get_clients {: bufnr :name :clangd})]
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

    (keymap :n "gh" switch-source-header {:buffer true})))

(let [lsp (require :lsp)]
  (lsp.start {:cmd ["clangd" "--background-index"]
              :root [".clangd" ".clang-format" "compile_commands.json" "compile_flags.txt"]
              :flags {:debounce_text_changes 20}
              :capabilities {:textDocument {:completion {:editsNearCursor true}}
                             :offsetEncoding [:utf-8 :utf-16]}}))
