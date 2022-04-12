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
    (fn [{: buf}]
      (vim.diagnostic.disable 0)
      (autocmd :BufWritePost {:once true :buffer buf}
        (fn [{: buf}]
          (vim.diagnostic.enable 0)
          (autocmd :InsertEnter {:buffer buf} #(vim.diagnostic.hide ns buf))
          (autocmd :CursorMoved {:buffer buf}
            (fn [{: buf}]
              (timer:stop)
              (show-cursor-diagnostics buf)))
          (autocmd [:InsertLeave :DiagnosticChanged] {:buffer buf}
            (fn [{: buf}]
              (let [{: mode} (nvim.get_mode)]
                (when (not= :i (mode:sub 1 1))
                  (show-cursor-diagnostics buf)))))))))
  (autocmd :DiagnosticChanged "*"
    (fn [{: buf}]
      (when (nvim.buf.is_loaded buf)
        (let [diagnostics (vim.diagnostic.toqflist (vim.diagnostic.get buf))]
          (each [_ winid (ipairs (vim.fn.win_findbuf buf))]
            (vim.fn.setloclist winid [] " " {:items diagnostics
                                             :title "Diagnostics"})))))))

(keymap :n "]g" #(vim.diagnostic.goto_next {:float false}))
(keymap :n "[g" #(vim.diagnostic.goto_prev {:float false}))
