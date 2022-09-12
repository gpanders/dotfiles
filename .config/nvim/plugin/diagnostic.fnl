(local ns (nvim.create_namespace :diagnostics))
(local diagnostics {})

(vim.diagnostic.config {:virtual_text false
                        :underline true
                        :signs true
                        :severity_sort true})

(fn cursor-diagnostic [buf]
  "Find the diagnostic closest to the cursor"
  (when (not (. diagnostics buf))
    (tset diagnostics buf (vim.diagnostic.get buf)))
  (let [[lnum curcol] (nvim.win_get_cursor 0)
        lnum (- lnum 1)]
    (var score math.huge)
    (var diag nil)
    (var done? false)
    (each [_ v (ipairs (. diagnostics buf) :until done?)]
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

(augroup diagnostics#
  (autocmd [:BufRead :BufNewFile]
    (fn [{: buf}]
      (vim.diagnostic.disable buf)
      (autocmd :BufWritePost {:once true :buffer buf}
        (fn [{: buf}]
          (vim.diagnostic.enable buf)
          (autocmd :InsertEnter {:buffer buf} #(vim.diagnostic.hide ns buf))
          (autocmd :CursorMoved {:buffer buf}
            (fn [{: buf}]
              (let [[lnum] (nvim.win_get_cursor 0)
                    lnum (- lnum 1)]
                (let [diag (cursor-diagnostic buf)]
                  (vim.diagnostic.show ns buf [diag] {:virtual_text true})))))))))
  (autocmd :DiagnosticChanged
    (fn [{: buf}]
      (tset diagnostics buf (vim.diagnostic.get buf))
      (when (nvim.buf_is_loaded buf)
        (let [items (vim.diagnostic.toqflist (. diagnostics buf))]
          (each [_ winid (ipairs (vim.fn.win_findbuf buf))]
            (let [action (match (vim.fn.getloclist winid {:context 1})
                            {:context {:diagnostics true}} "r"
                            _ " ")]
              (vim.fn.setloclist winid [] action {: items
                                                  :title :Diagnostics
                                                  :context {:diagnostics true}}))))))))

(keymap :n "]g" #(vim.diagnostic.goto_next {:float false}))
(keymap :n "[g" #(vim.diagnostic.goto_prev {:float false}))
(keymap :n "go" #(vim.diagnostic.open_float {:border :rounded}))
