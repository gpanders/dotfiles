(autocmd gitsigns# [:BufNewFile :BufRead] "*"
  #(let [(ok? result) (pcall vim.call :FugitiveGitDir)]
    (when (or (not ok?) (not= result ""))
      (with-module [gitsigns :gitsigns]
        (gitsigns.setup {:on_attach (fn [buffer]
                                      (keymap :n "[c" #(if vim.wo.diff
                                                           "[c"
                                                           (do
                                                             (vim.schedule #(gitsigns.prev_hunk))
                                                             "<Ignore>"))
                                              {: buffer :expr true})
                                      (keymap :n "]c" #(if vim.wo.diff
                                                           "]c"
                                                           (do
                                                             (vim.schedule #(gitsigns.next_hunk))
                                                             "<Ignore>"))
                                              {: buffer :expr true}))})
        true))))
