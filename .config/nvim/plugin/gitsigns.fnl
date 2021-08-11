(autocmd :my-gitsigns [:BufNewFile :BufRead] "*" []
  (let [(ok result) (pcall vim.api.nvim_call_function :FugitiveGitDir)]
    (when (or (not ok) (not= result ""))
      (let [(ok gitsigns) (pcall require :gitsigns)]
        (when ok
          (gitsigns.setup)
          (exec "autocmd! my-gitsigns"))))))
