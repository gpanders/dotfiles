(fn setup [{: args}]
  (nvim.del_user_command "Telescope")
  (vim.cmd "packadd telescope.nvim")
  (with-module [telescope :telescope]
    (telescope.load_extension "fzy_native"))
  (vim.cmd (.. "Telescope " args)))

(command :Telescope {:nargs "*"} setup)

(keymap :n "<Space>ff" "<Cmd>Telescope find_files theme=dropdown previewer=false hidden=true follow=true<CR>")
(keymap :n "<Space>b" "<Cmd>Telescope buffers theme=dropdown previewer=false<CR>")
(keymap :n "<Space>fo" "<Cmd>Telescope oldfiles theme=dropdown previewer=false<CR>")
(keymap :n "<Space>fl" "<Cmd>Telescope loclist<CR>")
(keymap :n "<Space>fq" "<Cmd>Telescope quickfix<CR>")

(augroup telescope#
  (autocmd [:VimEnter :BufRead :BufNewFile :DirChanged] "*"
    #(match (vim.fn.FugitiveGitDir)
       "" nil
       _ (do
           (keymap :n "<Space>ff" "<Cmd>Telescope git_files theme=dropdown previewer=false show_untracked=false use_git_root=false<CR>" {:buffer true})
           (keymap :n "<Space>fF" "<Cmd>Telescope find_files theme=dropdown previewer=false hidden=true follow=true<CR>" {:buffer true}))))
  (autocmd :User :LspAttached
    #(keymap :n "<Space>fs" "<Cmd>Telescope lsp_dynamic_workspace_symbols theme=dropdown previewer=false<CR>" {:buffer true}))
  (autocmd :FileType :TelescopePrompt
    (fn []
      (nvim.buf.del_keymap 0 :i "<C-U>")
      (keymap :i "<Esc>" "<C-C>" {:buffer true :noremap false}))))
