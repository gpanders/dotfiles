(when (= 0 (vim.fn.executable "fzy"))
  (exec "packadd telescope.nvim")
  (lua "return"))

(with-module [fzy :fzy]
  (let [height 20]
    (set fzy.opts {:number_items false
                   :fzy_opts "-i"
                   :popup_opts (fn [lines cols]
                                 {:relative :editor
                                  :style :minimal
                                  : height
                                  :width cols
                                  :col 0
                                  :row (- lines height)
                                  :border ["" "─" "" "" "" "" "" ""]})}))
  (fn oldfiles []
    (let [oldfiles (icollect [_ v (ipairs vim.v.oldfiles)]
                     (if (and
                           (not (v:find "COMMIT_EDITMSG"))
                           (not (v:find "vim/runtime/doc"))
                           (not (v:find "^man://"))
                           (not (v:find vim.env.TMPDIR)))
                         v))]
      (fzy.pick_one oldfiles "Oldfiles> " #$1 fzy.sinks.edit_file)))

  (fn commands []
    (let [cmds (icollect [k v (pairs (vim.api.nvim_get_commands {:builtin false}))]
                 (match v.nargs
                   (where (or :? :0)) k))]
      (fzy.pick_one cmds "Commands> " #$1 vim.cmd)))

  (fn spell-suggest []
    (let [word (vim.fn.expand "<cword>")
          suggestions (vim.fn.spellsuggest word)
          tmp (vim.fn.tempname)]
      (fzy.pick_one suggestions (: "Change '%s' to: " :format word) #$1 #(exec (.. "normal! ciw" (vim.trim $1))))))

  (keymap :n "<Space>f" #(fzy.execute "fd" fzy.sinks.edit_file))
  (keymap :n "<Space>b" #(fzy.actions.buffers))
  (keymap :n "<Space>t" #(fzy.try fzy.actions.lsp_tags fzy.actions.buf_tags))
  (keymap :n "<Space>q" #(fzy.actions.quickfix))
  (keymap :n "<Space>o" oldfiles)
  (keymap :n "<Space>g" #(fzy.execute "rg --no-heading --smart-case --column --line-number --color=never ." fzy.sinks.edit_live_grep))
  (keymap :n "z=" spell-suggest)
  (keymap :n "<M-x>" commands)

  (augroup fzy#
    (autocmd [:VimEnter :BufRead :BufNewFile] "*"
      (let [(ok? result) (pcall vim.call :FugitiveGitDir)]
        (when (and ok? (not= result ""))
          (keymap :n "<Space>f" #(fzy.execute "git ls-files" fzy.sinks.edit_file) {:buffer true})
          (keymap :n "<Space>F" #(fzy.execute "fd" fzy.sinks.edit_file) {:buffer true}))))))
