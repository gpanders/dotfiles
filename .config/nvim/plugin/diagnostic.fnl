(local api vim.api)
(local diagnostic vim.diagnostic)
(local ns (api.nvim_create_namespace :diagnostics))

(diagnostic.config {:virtual_text false
                    :virtlines false
                    :underline false
                    :severity_sort true})

(set diagnostic.handlers.virtlines {})

(fn diagnostic.handlers.virtlines.show [ns bufnr diagnostics opts]
  (let [ns (diagnostic.get_namespace ns)]
    (when (not ns.user_data.virtlines_ns)
      (tset ns :user_data :virtlines_ns (api.nvim_create_namespace "")))
    (let [virtlines-ns ns.user_data.virtlines_ns]
      (each [_ v (ipairs diagnostics)]
        (let [{: lnum : message : source : severity : col} v
              width (api.nvim_win_get_width 0)
              severity (. diagnostic.severity severity)
              virt-text-hl (.. :DiagnosticVirtualText severity)
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
          (api.nvim_buf_set_extmark bufnr virtlines-ns lnum col {:virt_lines virt-lines}))))))

(fn diagnostic.handlers.virtlines.hide [ns bufnr]
  (match (diagnostic.get_namespace ns)
    {:user_data {: virtlines_ns}} (api.nvim_buf_clear_namespace bufnr virtlines_ns 0 -1)))

(var show-virtlines? false)
(fn show-cursor-diagnostics [bufnr]
  (let [[lnum] (api.nvim_win_get_cursor 0)
        lnum (- lnum 1)
        diagnostics (diagnostic.get bufnr {: lnum})]
    (diagnostic.show ns bufnr diagnostics {:virtlines show-virtlines? :underline true})))

(autocmd diagnostics [:BufRead :BufNewFile] "*"
  (diagnostic.disable 0)
  (autocmd diagnostics :BufWritePost "<buffer=abuf>" :once
    (diagnostic.enable 0)
    (augroup diagnostics
      (autocmd [:CursorMoved :InsertLeave :DiagnosticChanged] "<buffer=abuf>"
        (let [bufnr (tonumber (vim.fn.expand "<abuf>"))
              {: mode} (api.nvim_get_mode)]
          (when (not= :i (mode:sub 1 1))
            (show-cursor-diagnostics bufnr))))
      (autocmd :InsertEnter "<buffer=abuf>"
        (let [bufnr (tonumber (vim.fn.expand "<abuf>"))]
          (diagnostic.handlers.virtlines.hide ns bufnr))))))

(autocmd diagnostics :DiagnosticChanged "*"
  (let [bufnr (tonumber (vim.fn.expand "<abuf>"))]
    (when (api.nvim_buf_is_loaded bufnr)
      (each [_ winid (ipairs (vim.fn.win_findbuf bufnr))]
        (diagnostic.setloclist {: winid :open false})))))

(keymap :n "]g" #(diagnostic.goto_next {:float false}))
(keymap :n "[g" #(diagnostic.goto_prev {:float false}))
(keymap :n "yog" (fn []
                   (set show-virtlines? (not show-virtlines?))
                   (show-cursor-diagnostics 0)))
