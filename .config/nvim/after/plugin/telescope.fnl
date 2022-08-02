(command :Telescope {:nargs "*"}
  (fn [args]
    (nvim.command "delcommand Telescope")
    (nvim.command "packadd telescope.nvim")
    (with-module [telescope :telescope]
      (telescope.setup {:pickers {:buffers {:sort_mru true}
                                  :git_files {:show_untracked false
                                              :use_git_root false}}
                        :defaults {:layout_strategy :vertical
                                   :dynamic_preview_title true
                                   :mappings {:i {"<Esc>" :close
                                                  "<C-D>" false
                                                  "<C-U>" false
                                                  "<C-Down>" :preview_scrolling_down
                                                  "<C-Up>" :preview_scrolling_up}}}})
      (telescope.load_extension :fzf))
    (vim.cmd (: "Telescope %s" :format args.args))))

(keymap :n "<C-P>" "<Cmd>Telescope find_files<CR>")
(keymap :n "<Space>b" "<Cmd>Telescope buffers<CR>")
(keymap :n "<Space>fo" "<Cmd>Telescope oldfiles<CR>")
(keymap :n "<Space>fl" "<Cmd>Telescope loclist<CR>")
(keymap :n "<Space>fq" "<Cmd>Telescope quickfix<CR>")
(keymap :n "<Space>fd" "<Cmd>Telescope diagnostics<CR>")
(keymap :n "<Space>/" "<Cmd>Telescope live_grep<CR>")
(keymap :n "<Space>*" "<Cmd>Telescope grep_string<CR>")

(augroup telescope#
  (autocmd [:VimEnter :BufRead :BufNewFile :DirChanged] "*"
    #(match (pcall vim.fn.FugitiveGitDir)
       false nil
       (true "") nil
       (true _) (do
                  (keymap :n "<C-P>" "<Cmd>Telescope git_files<CR>" {:buffer true})
                  (keymap :n "<Space>ff" "<Cmd>Telescope find_files<CR>" {:buffer true}))))
  (autocmd :LspAttach
    #(keymap :n "<Space>fs" "<Cmd>Telescope lsp_dynamic_workspace_symbols<CR>" {:buffer true})))
