(local opts {:fzf_opts {"--layout" false "--info" false}
             :keymap {:builtin {"<C-/>" :toggle-help}}
             :git {:files {:cmd "git ls-files -o -c --exclude-standard"}}})

(local fzf (setmetatable {} {:__index (fn [t k]
                                        (let [fzf (require :fzf-lua)]
                                          (fzf.setup opts))
                                        (setmetatable t {:__index (fn [t k]
                                                                    (tset t k (assert (. (require :fzf-lua) k)))
                                                                    (. t k))})
                                        (. t k))}))

(keymap :n "<Space>f" #(fzf.files))
(keymap :n "<Space>b" #(fzf.buffers))
(keymap :n "<Space>'" #(fzf.resume))
(keymap :n "<Space>g" #(fzf.diagnostics_document))
(keymap :n "<Space>G" #(fzf.diagnostics_workspace))
(keymap :n "<Space>/" #(fzf.live_grep_native {:prompt "> "}))
(keymap :n "<Space>*" #(fzf.grep_cword {:prompt "> "}))
(keymap :n "<Space>j" #(fzf.jumps))
(keymap :n "<Space>m" #(fzf.marks))
(command :Oldfiles {} #(fzf.oldfiles))

(augroup fzf#
  (autocmd [:VimEnter :BufRead :BufNewFile :DirChanged]
    #(case (pcall vim.fn.FugitiveGitDir)
       (true "") nil
       (true _) (do
                  (keymap :n "<Space>f" #(fzf.git_files {:prompt "> " :cwd (vim.uv.cwd)}) {:buffer true})
                  (keymap :n "<Space>F" #(fzf.files)) {:buffer true})))
  (autocmd :LspAttach
    #(keymap :n "<Space>s" #(fzf.lsp_live_workspace_symbols) {:buffer true}))
  (autocmd :LspDetach
    #(pcall nvim.buf_del_keymap 0 :n "<Space>s")))
