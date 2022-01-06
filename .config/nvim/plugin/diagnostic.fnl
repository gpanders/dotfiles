(local api vim.api)
(local ns (api.nvim_create_namespace :diagnostics))

(var diagnostics-enabled? true)

(vim.diagnostic.config {:virtual_text false
                        :underline false
                        :severity_sort true})

(fn show-cursor-diagnostics [bufnr]
  (api.nvim_buf_clear_namespace bufnr ns 0 -1)
  (let [[lnum] (api.nvim_win_get_cursor 0)
        lnum (- lnum 1)
        diagnostics (vim.diagnostic.get bufnr {: lnum})
        line-length (length (api.nvim_get_current_line))]
    (var diagnostic nil)
    (each [_ v (ipairs diagnostics)]
      (if (or (= nil diagnostic) (< v.severity diagnostic.severity))
          (set diagnostic v)))
    (when (and diagnostic (<= diagnostic.col diagnostic.end_col line-length))
      (let [{: message : source : severity : col : end_col} diagnostic
            width (api.nvim_win_get_width 0)
            severity (. vim.diagnostic.severity severity)
            virt-text-hl (.. :DiagnosticVirtualText severity)
            underline-hl (.. :DiagnosticUnderline severity)
            message (if source
                        (: "%s [%s]" :format message source)
                        message)
            max-line-length (accumulate [max 0 v (vim.gsplit message "\n")]
                              (math.max max (length v)))
            virtcol (-> (api.nvim_get_current_line)
                        (string.sub 1 col)
                        (vim.fn.strdisplaywidth))
            indent (string.rep " " virtcol)
            needs-wrap (< (- width virtcol 3) max-line-length)
            wrap-width (- width virtcol 3)
            break-pat (.. "[" vim.o.breakat "]")
            virt-lines []]
        (var first-line true)
        (each [v (vim.gsplit message "\n")]
          (var w nil)
          (var text v)
          (while (< 0 (length text))
            (set w (math.min (length text) wrap-width))
            (while (and (< 0 w (length text)) (not (string.find (text:sub w w) break-pat)))
              (set w (- w 1)))
            (when (<= w 0)
              (set w (length text)))
            (table.insert virt-lines [[(: "%s%s %s" :format
                                          indent
                                          (if first-line "└" " ")
                                          (text:sub 1 w))
                                       virt-text-hl]])
            (set first-line false)
            (set text (text:sub (+ w 1)))))
        (api.nvim_buf_set_extmark bufnr ns lnum col {: end_col
                                                     :hl_group underline-hl
                                                     :virt_lines virt-lines})))))
(autocmd diagnostics [:BufRead :BufNewFile] "*"
  (vim.diagnostic.disable 0)
  (autocmd diagnostics :BufWritePost "<buffer=abuf>" :once
    (vim.diagnostic.enable 0)
    (augroup diagnostics
      (autocmd [:CursorMoved :InsertLeave :DiagnosticChanged] "<buffer=abuf>"
        (when diagnostics-enabled?
          (let [bufnr (tonumber (vim.fn.expand "<abuf>"))
                {: mode} (api.nvim_get_mode)]
            (when (not= :i (mode:sub 1 1))
              (show-cursor-diagnostics bufnr)))))
      (autocmd :InsertEnter "<buffer=abuf>"
        (let [bufnr (tonumber (vim.fn.expand "<abuf>"))]
          (api.nvim_buf_clear_namespace bufnr ns 0 -1))))))

(keymap :n "yog" (fn []
                   (set diagnostics-enabled? (not diagnostics-enabled?))
                   (if diagnostics-enabled?
                       (do
                         (vim.diagnostic.config {:signs true})
                         (show-cursor-diagnostics 0))
                       (do
                         (vim.diagnostic.config {:signs false})
                         (api.nvim_buf_clear_namespace 0 ns 0 -1)))))

(autocmd diagnostics :DiagnosticChanged "*"
  (let [bufnr (tonumber (vim.fn.expand "<abuf>"))]
    (when (api.nvim_buf_is_loaded bufnr)
      (each [_ winid (ipairs (vim.fn.win_findbuf bufnr))]
        (vim.diagnostic.setloclist {: winid :open false})))))

(keymap :n "]g" #(vim.diagnostic.goto_next {:float false}))
(keymap :n "[g" #(vim.diagnostic.goto_prev {:float false}))
