(when (= vim.g.loaded_fugitive 1)
  (augroup :my-fugitive
    (autocmd :BufRead "fugitive://*" []
      (setlocal bufhidden :delete))
    (autocmd :FileType "fugitive,fugitiveblame" []
      (keymap :n :q :gq {:silent true :buffer true :noremap false}))
    (autocmd :BufRead "*" []
      (let [$HOME (os.getenv :HOME)]
        (when (and (= (vim.fn.getcwd) $HOME) (empty-or-nil? vim.b.git_dir))
          (set vim.b.git_dir (.. $HOME "/.dotfiles"))))))
  (keymap :n "g<Space>" ":Git<Space>")
  (keymap :n "g<CR>" "<Cmd>Git<CR>"))

