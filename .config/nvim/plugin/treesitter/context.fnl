(local {: node-at-cursor : containing-function-node} (require :treesitter))

(local state {})

(fn clear []
  (when state.winid
    (vim.api.nvim_win_close state.winid true))
  (set state.winid nil)
  (set state.node nil))

(fn show-function-scope []
  (let [bufnr (vim.api.nvim_get_current_buf)
        winid (vim.api.nvim_get_current_win)]
    (match (containing-function-node bufnr)
      (node text)
      (let [start-row (node:range)
            {:row screen-row} (vim.fn.screenpos winid (+ start-row 1) 1)]
        (if (= screen-row 0)
            (when (not= (node:id) state.node)
              (clear)
              (let [tmpbuf (vim.api.nvim_create_buf false true)
                    floatwin (vim.api.nvim_open_win tmpbuf false {:relative :win
                                                                  :win winid
                                                                  :row 0
                                                                  :col 0
                                                                  :width (vim.fn.winwidth winid)
                                                                  :height 1
                                                                  :focusable false
                                                                  :style :minimal
                                                                  :noautocmd true})]
                (vim.api.nvim_buf_set_lines tmpbuf 0 -1 true [text])
                (tset vim.bo tmpbuf :filetype (. vim.bo bufnr :filetype))
                (tset vim.wo floatwin :winhighlight "NormalFloat:TreesitterScope")
                (set state.node (node:id))
                (set state.winid floatwin)))
            (clear)))
      _ (clear))))

(autocmd treesitter# :FileType "*"
  (let [bufnr (tonumber (vim.fn.expand "<abuf>"))
        lang (. vim.bo bufnr :filetype)]
    (when (vim.treesitter.language.require_language lang nil true)
      (autocmd treesitter# [:WinScrolled :CursorMoved] "<buffer=abuf>" (show-function-scope)))))
