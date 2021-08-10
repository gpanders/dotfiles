(autocmd :my-gitsigns [:BufNewFile :BufRead] "*" []
  (local (ok gitsigns) (pcall require :gitsigns))
  (when ok
    (gitsigns.setup)
    (exec "autocmd! my-gitsigns")))
