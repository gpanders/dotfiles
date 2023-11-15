(local state {:timer (vim.uv.new_timer)
              :highlight nil})

(fn utf-index [line index encoding]
  "Convert the byte index in the given line into the corresponding UTF code unit index."
  (if (= :utf-8 encoding)
      index
      (or (= :utf-16 encoding) (= :utf-32 encoding))
      (let [(u16 u32) (vim.str_utfindex line index)]
        (if (= :utf-16 encoding)
            u16
            u32))
      (error "Invalid encoding")))

(fn mouse-hover [buf client]
  (let [{:line row :column col : winid} (vim.fn.getmousepos)]
    (when (= buf (nvim.win_get_buf winid))
      (let [row (- row 1)
            [line] (nvim.buf_get_lines buf row (+ row 1) true)]
        (when (and line (<= col (length line)))
          (let [uri (vim.uri_from_bufnr buf)
                character (utf-index line col client.offset_encoding)
                params {:textDocument {: uri}
                        :position {:line row : character}}
                hover "textDocument/hover"
                document-highlight "textDocument/documentHighlight"]
            (when (client.supports_method hover {:bufnr buf})
              (let [handler (or (. client.handlers hover) vim.lsp.handlers.hover)
                    handler (vim.lsp.with handler {:relative :mouse :silent true})]
                (client.request hover params handler buf)))
            (when (client.supports_method document-highlight {:bufnr buf})
              (let [handler (or (. client.handlers document-highlight)
                                #(vim.lsp.util.buf_highlight_references buf (or $2 []) client.offset_encoding))
                    handler #(do
                               (vim.lsp.util.buf_clear_references buf)
                               (handler $...)
                               (set state.highlight buf))]
                (client.request document-highlight params handler buf)))))))))

(fn mouse-move [buf client]
  ; Clear any existing hover window
  (case (. vim.b buf :lsp_floating_preview)
    w (do
        (when (nvim.win_is_valid w)
          (nvim.win_close w true))
        (tset vim.b buf :lsp_floating_preview nil)))

  ; Clear any existing highlights
  (case state.highlight
    b (do
        (vim.lsp.util.buf_clear_references b)
        (set state.highlight nil)))

  (state.timer:stop)
  (state.timer:start 2000 0 #(vim.schedule #(mouse-hover buf client))))

(autocmd :LspAttach "*"
  (fn [{: buf :data {: client_id}}]
    (let [client (vim.lsp.get_client_by_id client_id)]
      (when (or (client.supports_method "textDocument/hover" {:bufnr buf})
                (client.supports_method "textDocument/documentHighlight" {:bufnr buf}))
        (keymap :n "<MouseMove>" #(mouse-move buf client) {:buffer buf})))))

(set vim.o.mousemoveevent true)
