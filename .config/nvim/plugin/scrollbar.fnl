(local ns (vim.api.nvim_create_namespace "scrollbar"))
(local state {})

(fn on-win [_ winid bufnr topline botline]
  (if (not= "" (. (vim.api.nvim_win_get_config winid) :relative))
      false
      (let [lines (vim.api.nvim_buf_line_count bufnr)
            height (vim.api.nvim_win_get_height winid)]
        (if (<= lines height)
            (tset state winid nil)
            (let [visible (+ (- botline topline) 1)
                  span (math.max 1 (math.floor (+ 0.5 (* (/ visible lines) height))))
                  offset (math.floor (+ 0.5 (* (/ topline (math.max 1 (- lines visible)))
                                               (- height span))))
                  start (math.max topline
                                  (math.min (- (+ botline 1) span)
                                            (+ topline offset)))
                  end (+ start span)]
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
