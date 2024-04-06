(macro setup [mod ?opts ...]
  `(with-module [m# ,mod]
     ((. m# :setup) ,(or ?opts {}))
     ,...))

(setup :mini.sessions)
(setup :mini.surround)
(setup :mini.align {:mappings {:start "gl" :start_with_preview "gL"}})
(setup :mini.files {:content {:prefix #nil} :mappings {:go_in_plus :<CR>}}
  (keymap :n "-" #(MiniFiles.open (nvim.buf_get_name 0))))
(setup :mini.visits)
(setup :mini.extra)
(setup :mini.diff {:view {:signs {:add :┃ :change :┃ :delete :▁}}
                   :delay {:text_change 50}}
  (keymap :n "yoD" MiniDiff.toggle_overlay))

(autocmd mini# :LspAttach {:once true}
  #(setup :mini.notify {:content {:format #(. $ :msg)}
                        :window {:winblend 0
                                 :config #{:anchor "SE"
                                           :row (- vim.o.lines vim.o.cmdheight (math.min 1 vim.o.laststatus))}}}))

(setup :mini.pick {:mappings {:toggle_info "<C-/>"}}
  (set MiniPick.config.source.show MiniPick.default_show)
  (keymap :n "<Space>f" MiniPick.builtin.files)
  (keymap :n "<Space>/" MiniPick.builtin.grep_live)
  (keymap :n "<Space>b" MiniPick.builtin.buffers)
  (keymap :n "<M-S-/>" MiniPick.builtin.help)

  (when MiniExtra
    (keymap :n "<Space>r" MiniExtra.pickers.visit_paths)
    (keymap :n "<Space>g" #(MiniExtra.pickers.diagnostic {:get_opts {:severity {:min vim.diagnostic.severity.WARN}}}))
    (augroup mini#
      (autocmd :LspAttach "*" #(keymap :n "<Space>s" #(MiniExtra.pickers.lsp {:scope :workspace_symbol}) {:buffer true}))
      (autocmd :LspDetach "*" #(vim.keymap.del :n "<Space>s" {:buffer true}))))

  (augroup mini#
    (autocmd [:VimEnter :BufRead :BufNewFile :DirChanged]
      #(case (pcall vim.fn.FugitiveGitDir)
         (true "") nil
         (true _) (do
                    (keymap :n "<Space>f" #(MiniPick.builtin.files {:tool :git}) {:buffer true})
                    (keymap :n "<Space>F" MiniPick.builtin.files {:buffer true}))))))

