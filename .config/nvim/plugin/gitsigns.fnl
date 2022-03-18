(autocmd gitsigns# [:BufNewFile :BufRead] "*"
  (let [(ok? result) (pcall vim.call :FugitiveGitDir)]
    (when (or (not ok?) (not= result ""))
      (with-module [gitsigns :gitsigns]
        (gitsigns.setup)
        (exec "autocmd! gitsigns#")))))
