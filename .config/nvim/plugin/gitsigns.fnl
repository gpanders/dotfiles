(autocmd gitsigns# [:BufNewFile :BufRead] "*"
  #(let [(ok? result) (pcall vim.call :FugitiveGitDir)]
    (when (or (not ok?) (not= result ""))
      (with-module [gitsigns :gitsigns]
        (gitsigns.setup)
        (keymap :n "[c" #(if vim.o.diff
                             (vim.cmd "normal! [c")
                             (gitsigns.prev_hunk)))
        (keymap :n "]c" #(if vim.o.diff
                             (vim.cmd "normal! ]c")
                             (gitsigns.next_hunk)))
        true))))
