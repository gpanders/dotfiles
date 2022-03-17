(when vim.g.loaded_telescope
  (keymap :n "<Space>f" "<Cmd>Telescope find_files theme=dropdown previewer=false hidden=true follow=true<CR>")
  (keymap :n "<Space>g" "<Cmd>Telescope live_grep<CR>")
  (keymap :n "<Space>b" "<Cmd>Telescope buffers theme=dropdown previewer=false<CR>")
  (keymap :n "<Space>o" "<Cmd>Telescope oldfiles theme=dropdown previewer=false<CR>")
  (keymap :n "z=" "<Cmd>Telescope spell_suggest theme=cursor<CR>")

  (augroup telescope#
    (autocmd [:VimEnter :BufRead :BufNewFile :DirChanged] "*"
      (let [(ok? result) (pcall vim.call :FugitiveGitDir)]
        (when (and ok? (not= result "") (<= (length (dirname result)) (length (vim.loop.cwd))))
          (keymap :n "<Space>f" "<Cmd>Telescope git_files theme=dropdown previewer=false show_untracked=false use_git_root=false<CR>" {:buffer true})
          (keymap :n "<Space>F" "<Cmd>Telescope find_files theme=dropdown previewer=false hidden=true follow=true<CR>" {:buffer true}))))
    (autocmd :User :LspAttached
      (keymap :n "<Space>t" "<Cmd>Telescope lsp_dynamic_workspace_symbols theme=dropdown previewer=false<CR>" {:buffer true}))))
