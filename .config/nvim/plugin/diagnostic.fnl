(local ns (nvim.create_namespace :diagnostics))
(local state {})

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

(nvim.set_decoration_provider ns {:on_end #(match state.diagnostic
                                             {: source : message} (print (if source
                                                                             (: "%s: %s" :format source message)
                                                                             message))
                                             _ (echo ""))})

(augroup diagnostics#
  (autocmd [:BufRead :BufNewFile]
    (fn [{: buf}]
      (vim.diagnostic.disable 0)
      (autocmd :BufWritePost {:once true :buffer buf}
        (fn [{: buf}]
          (vim.diagnostic.enable 0)
          (autocmd :InsertEnter {:buffer buf} #(set state.diagnostic nil))
          (autocmd :CursorMoved {:buffer buf}
            (fn [{: buf}]
              (let [[lnum] (nvim.win.get_cursor 0)
                    lnum (- lnum 1)]
                (set state.diagnostic (cursor-diagnostic (vim.diagnostic.get buf {: lnum}))))))))))
  (autocmd :DiagnosticChanged
    (fn [{: buf}]
      (when (nvim.buf.is_loaded buf)
        (let [diagnostics (vim.diagnostic.toqflist (vim.diagnostic.get buf))]
          (each [_ winid (ipairs (vim.fn.win_findbuf buf))]
            (let [action (match (vim.fn.getloclist winid {:context 1})
                            {:context {:diagnostics true}} "r"
                            _ " ")]
              (vim.fn.setloclist winid [] action {:items diagnostics
                                                  :title :Diagnostics
                                                  :context {:diagnostics true}}))))))))

(keymap :n "]g" #(vim.diagnostic.goto_next {:float false}))
(keymap :n "[g" #(vim.diagnostic.goto_prev {:float false}))
