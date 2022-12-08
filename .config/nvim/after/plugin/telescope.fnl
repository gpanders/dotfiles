(command :Telescope {:nargs "*"}
  (fn [args]
    (nvim.command "delcommand Telescope")
    (nvim.command "packadd telescope.nvim")
    (let [telescope (require :telescope)]
      (telescope.setup {:pickers {:buffers {:sort_mru true}
                                  :git_files {:show_untracked true}}
                        :defaults {:dynamic_preview_title true
                                   :mappings {:i {"<Esc>" :close
                                                  "<C-D>" false
                                                  "<C-U>" false
                                                  "<S-Down>" :preview_scrolling_down
                                                  "<S-Up>" :preview_scrolling_up}}}})
      (telescope.load_extension :fzy_native))
    (vim.cmd (: "Telescope %s" :format args.args))))

(keymap :n "<Space>f" "<Cmd>Telescope find_files<CR>")
(keymap :n "<Space>b" "<Cmd>Telescope buffers<CR>")
(command :Oldfiles {} "Telescope oldfiles")
(keymap :n "<Space>g" "<Cmd>Telescope diagnostics<CR>")
(keymap :n "<Space>/" "<Cmd>Telescope live_grep<CR>")
(keymap :n "<Space>*" "<Cmd>Telescope grep_string<CR>")

(augroup telescope#
  (autocmd [:VimEnter :BufRead :BufNewFile :DirChanged] "*"
    #(match (pcall vim.fn.FugitiveGitDir)
       false nil
       (true "") nil
       (true _) (do
                  (keymap :n "<Space>f" "<Cmd>Telescope git_files use_git_root=false<CR>" {:buffer true})
                  (keymap :n "<Space>F" "<Cmd>Telescope git_files<CR>" {:buffer true}))))
  (autocmd :LspAttach
    #(keymap :n "<Space>s" "<Cmd>Telescope lsp_dynamic_workspace_symbols<CR>" {:buffer true})))
