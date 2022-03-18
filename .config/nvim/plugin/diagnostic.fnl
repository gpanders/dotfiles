(local ns (nvim.create_namespace :diagnostics))

(vim.diagnostic.config {:virtual_text false
                        :underline true
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
        lnum (- lnum 1)
        diagnostic (cursor-diagnostic (vim.diagnostic.get bufnr {: lnum}))]
    (vim.diagnostic.show ns bufnr [diagnostic] {:virtual_text true})))

(augroup diagnostics#
  (autocmd [:BufRead :BufNewFile] "*"
    (vim.diagnostic.disable 0)
    (autocmd :BufWritePost "<buffer=abuf>" {:once true}
      (vim.diagnostic.enable 0)
      (autocmd [:CursorMoved :InsertLeave :DiagnosticChanged] "<buffer=abuf>"
        (let [bufnr (tonumber (vim.fn.expand "<abuf>"))
              {: mode} (nvim.get_mode)]
          (when (not= :i (mode:sub 1 1))
            (show-cursor-diagnostics bufnr))))))

  (autocmd :DiagnosticChanged "*"
    (let [bufnr (tonumber (vim.fn.expand "<abuf>"))]
      (when (nvim.buf.is_loaded bufnr)
        (let [diagnostics (vim.diagnostic.toqflist (vim.diagnostic.get bufnr))]
          (each [_ winid (ipairs (vim.fn.win_findbuf bufnr))]
            (vim.fn.setloclist 0 diagnostics)))))))

(keymap :n "]g" #(vim.diagnostic.goto_next {:float false}))
(keymap :n "[g" #(vim.diagnostic.goto_prev {:float false}))
