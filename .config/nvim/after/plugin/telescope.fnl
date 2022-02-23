(when vim.g.loaded_telescope
  (keymap :n "<Space>f" "<Cmd>Telescope find_files previewer=false hidden=true follow=true<CR>")
  (keymap :n "<Space>g" "<Cmd>Telescope live_grep<CR>")
  (keymap :n "<Space>b" "<Cmd>Telescope buffers previewer=false<CR>")
  (keymap :n "<Space>o" "<Cmd>Telescope oldfiles previewer=false<CR>")
  (keymap :n "z=" "<Cmd>Telescope spell_suggest<CR>")

  (augroup telescope#
    (autocmd [:VimEnter :BufRead :BufNewFile :DirChanged] "*"
      (let [(ok? result) (pcall vim.call :FugitiveGitDir)]
        (when (and ok? (not= result "") (<= (length (vim.fn.fnamemodify result ":h")) (length (vim.loop.cwd))))
          (keymap :n "<Space>f" "<Cmd>Telescope git_files previewer=false show_untracked=false use_git_root=false<CR>" {:buffer true})
          (keymap :n "<Space>F" "<Cmd>Telescope find_files previewer=false hidden=true follow=true<CR>" {:buffer true}))))
    (autocmd :User :LspAttached
      (keymap :n "<Space>t" "<Cmd>Telescope lsp_dynamic_workspace_symbols<CR>" {:buffer true}))))
