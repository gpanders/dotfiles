(when (= vim.g.loaded_fugitive 1)
  (augroup fugitive#
    (autocmd :BufRead "fugitive://*"
      (setlocal bufhidden :delete))
    (autocmd :FileType "fugitive,fugitiveblame"
      (keymap :n :q :gq {:buffer true :noremap false}))
    (autocmd :BufRead "*"
      (let [$HOME (os.getenv :HOME)]
        (when (= (vim.loop.cwd) $HOME)
          (vim.call :FugitiveDetect (.. $HOME "/.dotfiles"))))))
  (keymap :n "g<Space>" ":Git<Space>" {:silent false})
  (keymap :n "g<CR>" "<Cmd>Git<CR>"))

