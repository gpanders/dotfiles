(with-module [pick :mini.pick]
  (pick.setup {:source {:show pick.default_show}
               :mappings {:toggle_info "<C-/>"}})
  (keymap :n "<Space>f" pick.builtin.files)
  (keymap :n "<Space>b" pick.builtin.buffers)
  (keymap :n "<Space>/" pick.builtin.grep_live)
  (keymap :n "<M-S-/>" pick.builtin.help)

  (augroup mini#
    (autocmd [:VimEnter :BufRead :BufNewFile :DirChanged]
      #(case (pcall vim.fn.FugitiveGitDir)
         (true "") nil
         (true _) (do
                    (keymap :n "<Space>f" #(pick.builtin.files {:tool :git}) {:buffer true})
                    (keymap :n "<Space>F" pick.builtin.files {:buffer true}))))))
