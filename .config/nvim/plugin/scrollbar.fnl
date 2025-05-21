(local ns (vim.api.nvim_create_namespace "scrollbar"))
(local state {})

(fn on-win [_ winid bufnr topline botline]
  (if (not= "" (. (vim.api.nvim_win_get_config winid) :relative))
      false
      (let [lines (vim.api.nvim_buf_line_count bufnr)
            height (vim.api.nvim_win_get_height winid)]
        (if (<= lines height)
            (tset state winid nil)
            (let [cells-per-line (/ height lines)
                  span (math.floor (+ 0.5 (* (- botline topline) cells-per-line)))
                  start (math.floor (+ topline (* topline cells-per-line)))
                  end (math.min lines (+ start span 1))]
              (tset state winid {: start : end})))
        (vim.api.nvim__redraw {:win winid :valid false})
        (not= nil (. state winid)))))

(fn on-line [_ winid bufnr row]
  (case (. state winid)
    {: start : end} (when (and (<= start row) (< row end))
                      (vim.api.nvim_buf_set_extmark bufnr ns row 0 {:ephemeral true
                                                                    :virt_text [["▐" "WinSeparator"]]
                                                                    :virt_text_pos :right_align
                                                                    :virt_text_repeat_linebreak true}))))

(vim.api.nvim_set_decoration_provider ns {:on_win on-win :on_line on-line})
