(fn mouse-hover [buf client]
  (let [{:line row :column col : winid} (vim.fn.getmousepos)]
    (when (= buf (nvim.win_get_buf winid))
      (let [row (- row 1)
            [line] (nvim.buf_get_lines buf row (+ row 1) true)]
        (when (and line (<= col (length line)))
          (let [uri (vim.uri_from_bufnr buf)
                character (vim.str_utfindex line client.offset_encoding col)
                params {:textDocument {: uri}
                        :position {:line row : character}}
                hover "textDocument/hover"
                document-highlight "textDocument/documentHighlight"]
            (when (client:supports_method hover {:bufnr buf})
              (let [handler (or (. client.handlers hover) vim.lsp.handlers.hover)
                    handler (vim.lsp.with handler {:relative :mouse :silent true :border rounded})]
                (client:request hover params handler buf)))
            (when (client:supports_method document-highlight {:bufnr buf})
              (let [handler (or (. client.handlers document-highlight)
                                #(vim.lsp.util.buf_highlight_references buf (or $2 []) client.offset_encoding))
                    handler #(do
                               (vim.lsp.util.buf_clear_references buf)
                               (handler $...))]
                (client:request document-highlight params handler buf)))))))))

(augroup mouse#
  (autocmd :LspAttach "*"
    (fn [{: buf :data {: client_id}}]
      (let [client (vim.lsp.get_client_by_id client_id)]
        (when (or (client:supports_method "textDocument/hover" {:bufnr buf})
                  (client:supports_method "textDocument/documentHighlight" {:bufnr buf}))
          (keymap :n "<2-LeftMouse>" #(mouse-hover buf client) {:buffer buf}))))))
