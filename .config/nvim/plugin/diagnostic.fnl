(local ns (nvim.create_namespace :diagnostics))
(local timer (vim.loop.new_timer))

(vim.diagnostic.config {:virtual_text false
                        :underline true
                        :signs true
                        :severity_sort true})

(fn cursor-diagnostic [diagnostics]
  "Find the diagnostic closest to the cursor"
  (let [[lnum curcol] (nvim.win.get_cursor 0)
        lnum (- lnum 1)]
    (var score math.huge)
    (var diag nil)
    (var done? false)
    (each [_ v (ipairs diagnostics) :until done?]
      (match v
        {: lnum : col : end_col}
        (if (<= col curcol end_col)
            (do
              (set diag v)
              (set done? true))
            (let [scr (math.min (math.abs (- curcol col)) (math.abs (- curcol end_col)))]
              (when (< scr score)
                (set diag v)
                (set score scr))))))
    diag))

(fn show-cursor-diagnostics [bufnr]
  (let [[lnum] (nvim.win.get_cursor 0)
        lnum (- lnum 1)]
    (match (cursor-diagnostic (vim.diagnostic.get bufnr {: lnum}))
      diagnostic (timer:start 100 0 #(vim.schedule #(vim.diagnostic.show ns bufnr [diagnostic] {:virtual_text true})))
      nil (vim.diagnostic.hide ns bufnr))))

(augroup diagnostics#
  (autocmd [:BufRead :BufNewFile] "*"
    (vim.diagnostic.disable 0)
    (autocmd :BufWritePost "<buffer=abuf>" {:once true}
      (vim.diagnostic.enable 0)
      (autocmd :InsertEnter "<buffer=abuf>"
        (vim.diagnostic.hide ns bufnr))
      (autocmd :CursorMoved "<buffer=abuf>"
        (timer:stop)
        (show-cursor-diagnostics (tonumber (vim.fn.expand "<abuf>"))))
      (autocmd [:InsertLeave :DiagnosticChanged] "<buffer=abuf>"
        (let [{: mode} (nvim.get_mode)]
          (when (not= :i (mode:sub 1 1))
            (show-cursor-diagnostics (tonumber (vim.fn.expand "<abuf>"))))))))
  (autocmd :DiagnosticChanged "*"
    (let [bufnr (tonumber (vim.fn.expand "<abuf>"))]
      (when (nvim.buf.is_loaded bufnr)
        (let [diagnostics (vim.diagnostic.toqflist (vim.diagnostic.get bufnr))]
          (each [_ winid (ipairs (vim.fn.win_findbuf bufnr))]
            (vim.fn.setloclist winid [] " " {:items diagnostics
                                             :title "Diagnostics"})))))))

(keymap :n "]g" #(vim.diagnostic.goto_next {:float false}))
(keymap :n "[g" #(vim.diagnostic.goto_prev {:float false}))
