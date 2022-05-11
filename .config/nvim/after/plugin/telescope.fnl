(command :Telescope {:nargs "*"}
  (fn [args]
    (vim.cmd "packadd telescope.nvim")
    (with-module [telescope :telescope]
      (let [layout_config {:width #(math.min $2 100)
                           :height #(math.min $3 25)}]
        (telescope.setup {:pickers {:find_files {:previewer false
                                                 :theme :dropdown
                                                 : layout_config
                                                 :hidden true
                                                 :follow true}
                                    :buffers {:previewer false
                                              :theme :dropdown
                                              : layout_config}
                                    :git_files {:previewer false
                                                :theme :dropdown
                                                : layout_config
                                                :show_untracked false
                                                :use_git_root false}}})))
    (vim.cmd (: "Telescope %s" :format args.args))))

(keymap :n "<Space>ff" "<Cmd>Telescope find_files<CR>")
(keymap :n "<Space>b" "<Cmd>Telescope buffers<CR>")
(keymap :n "<Space>fo" "<Cmd>Telescope oldfiles<CR>")
(keymap :n "<Space>fl" "<Cmd>Telescope loclist<CR>")
(keymap :n "<Space>fq" "<Cmd>Telescope quickfix<CR>")
(keymap :n "<Space>fd" "<Cmd>Telescope diagnostics<CR>")

(augroup telescope#
  (autocmd [:VimEnter :BufRead :BufNewFile :DirChanged] "*"
    #(match (pcall vim.fn.FugitiveGitDir)
       false nil
       (true "") nil
       (true _) (do
                  (keymap :n "<Space>ff" "<Cmd>Telescope git_files<CR>" {:buffer true})
                  (keymap :n "<Space>fF" "<Cmd>Telescope find_files<CR>" {:buffer true}))))
  (autocmd :User :LspAttached
    #(keymap :n "<Space>fs" "<Cmd>Telescope lsp_dynamic_workspace_symbols<CR>" {:buffer true}))
  (autocmd :FileType :TelescopePrompt
    (fn []
      (nvim.buf.del_keymap 0 :i "<C-U>")
      (keymap :i "<Esc>" "<C-C>" {:buffer true :noremap false}))))
