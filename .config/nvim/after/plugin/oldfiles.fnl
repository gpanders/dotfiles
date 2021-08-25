(when (= vim.g.loaded_oldfiles 1)
  (set vim.g.oldfiles_blacklist ["COMMIT_EDITMSG" "vim/runtime/doc"])
  (autocmd :my-oldfiles :FileType :qf
    (when (= vim.w.quickfix_title :Oldfiles)
      (keymap :n "<CR>" "<CR><Cmd>cclose<CR>" {:buffer true}))))
