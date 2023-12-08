(local ns (nvim.create_namespace :diagnostics))
(local cache {})

(vim.diagnostic.config {:virtual_text false
                        :underline true
                        :signs {:severity {:min vim.diagnostic.severity.INFO}}
                        :severity_sort true})

(fn cursor-diagnostic [buf]
  "Find the diagnostic closest to the cursor"
  (when (not (. cache buf))
    (tset cache buf (vim.diagnostic.get buf {:severity {:min vim.diagnostic.severity.INFO}})))
  (let [[lnum curcol] (nvim.win_get_cursor 0)
        lnum (- lnum 1)]
    (var score math.huge)
    (var diag nil)
    (var done? false)
    (each [_ v (ipairs (. cache buf) &until done?)]
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
      (autocmd :InsertEnter {:buffer buf} #(vim.diagnostic.hide ns buf))
      (autocmd :CursorMoved {:buffer buf}
        (fn [{: buf}]
          (let [[lnum] (nvim.win_get_cursor 0)
                lnum (- lnum 1)]
            (let [diag (cursor-diagnostic buf)]
              (when diag
                (set diag.message ((vim.gsplit diag.message "\n"))))
              (vim.diagnostic.show ns buf [diag] {:virtual_text {:source :if_many}})))))))
  (autocmd :DiagnosticChanged
    (fn [{: buf}]
      (tset cache buf nil))))

(keymap :n "]g" #(vim.diagnostic.goto_next {:float false :severity {:min vim.diagnostic.severity.WARN}}))
(keymap :n "[g" #(vim.diagnostic.goto_prev {:float false :severity {:min vim.diagnostic.severity.WARN}}))
(keymap :n "go" #(vim.diagnostic.open_float {:border :rounded}))

(var enabled true)
(keymap :n "yog" #(do (if enabled
                          (vim.diagnostic.disable)
                          (vim.diagnostic.enable))
                      (set enabled (not enabled))) {:desc "Toggle diagnostics"})
