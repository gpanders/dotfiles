(autocmd gitsigns# [:BufNewFile :BufRead] "*"
  (let [(ok? result) (pcall vim.api.nvim_call_function :FugitiveGitDir)]
    (when (or (not ok?) (not= result ""))
      (with-module [gitsigns :gitsigns]
        (gitsigns.setup)
        (exec "autocmd! gitsigns#")))))
