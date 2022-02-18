(local {: node-at-cursor : context} (require :treesitter))

(local state {})

(fn clear []
  (when state.winid
    (vim.api.nvim_win_close state.winid true))
  (set state.winid nil)
  (set state.node nil))

(fn show-context []
  (let [bufnr (vim.api.nvim_get_current_buf)
        winid (vim.api.nvim_get_current_win)
        [{: textoff}] (vim.fn.getwininfo winid)
        width (- (vim.api.nvim_win_get_width winid) textoff)]
    (match (context bufnr)
      ([ctx] text)
      (let [start-row (ctx:start)
            {:row screen-row} (vim.fn.screenpos winid (+ start-row 1) 1)]
        (if (= screen-row 0)
            (if (not= (ctx:id) state.node)
                (do
                  (clear)
                  (let [tmpbuf (vim.api.nvim_create_buf false true)
                        floatwin (vim.api.nvim_open_win tmpbuf false {:relative :win
                                                                      :win winid
                                                                      :row 0
                                                                      :col textoff
                                                                      : width
                                                                      :height 1
                                                                      :focusable false
                                                                      :style :minimal
                                                                      :noautocmd true})]
                    (vim.api.nvim_buf_set_lines tmpbuf 0 -1 true [text])
                    (tset vim.bo tmpbuf :modifiable false)
                    (tset vim.bo tmpbuf :readonly true)
                    (tset vim.bo tmpbuf :filetype (. vim.bo bufnr :filetype))
                    (tset vim.wo floatwin :winhighlight "NormalFloat:TreesitterContext")
                    (set state.node (ctx:id))
                    (set state.winid floatwin)))
                (vim.api.nvim_win_set_config state.winid {:relative :win
                                                          :row 0
                                                          :col textoff
                                                          : width}))
            (clear)))
      _ (clear))))

(autocmd treesitter# :FileType "*"
  (let [bufnr (tonumber (vim.fn.expand "<abuf>"))
        lang (. vim.bo bufnr :filetype)]
    (when (vim.treesitter.language.require_language lang nil true)
      (autocmd treesitter# [:WinScrolled :CursorMoved] "<buffer=abuf>" (show-context)))))
