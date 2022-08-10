(when (= 1 vim.g.loaded_fzf_lua)
  (var loaded false)
  (let [fzf (setmetatable {} {:__index (fn [t k]
                                         (with-module [fzf :fzf-lua]
                                            (when (not loaded)
                                              (set loaded true)
                                              (fzf.setup {:fzf_opts {"--layout" :default}}))
                                            (tset t k (. fzf k)))
                                         (. t k))})]
    (let [func vim.ui.select]
      (fn vim.ui.select [...]
        (set vim.ui.select func)
        (fzf.register_ui_select {:winopts {:height 20 :width 100}})
        (vim.ui.select ...)))
    (keymap :n "<C-P>" #(fzf.files))
    (keymap :n "<Space>b" #(fzf.buffers))
    (keymap :n "<Space>*" #(fzf.grep_cword))
    (keymap :n "<Space>/" #(fzf.live_grep_native))
    (command :Oldfiles {} #(fzf.oldfiles))
    (augroup fzf#
      (autocmd [:VimEnter :BufRead :BufNewFile :DirChanged] "*"
        #(match (pcall vim.fn.FugitiveGitDir)
           false nil
           (true "") nil
           (true _) (do
                      (keymap :n "<C-P>" #(fzf.git_files {:prompt "> " :cwd (vim.loop.cwd)}))
                      (keymap :n "<Space>ff" #(fzf.files)))))
      (autocmd :LspAttach
        #(keymap :n "<Space>fs" #(fzf.lsp_live_workspace_symbols))))))
