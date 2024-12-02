(macro setup [mod ?opts ...]
  `(with-module [m# ,mod]
     ((. m# :setup) ,(or ?opts {}))
     ,...))

(setup :mini.sessions)

; Use vim-surround mappings, :h MiniSurround-vim-surround-config
(setup :mini.surround {:mappings {:add "ys"
                                  :delete "ds"
                                  :find ""
                                  :find_left ""
                                  :highlight ""
                                  :replace "cs"
                                  :update_n_lines ""
                                  :suffix_last ""
                                  :suffix_next ""
                                  :search_method "cover_or_next"}}
  (vim.keymap.del :x "ys")
  (keymap :x "S" ":<C-U>lua MiniSurround.add('visual')<CR>")
  (keymap :n "yss" "ys_" {:noremap false}))

(setup :mini.align {:mappings {:start "gl" :start_with_preview "gL"}})
(setup :mini.files {:content {:prefix #nil} :mappings {:go_in_plus :<CR>}}
  (keymap :n "-" #(MiniFiles.open (nvim.buf_get_name 0))))
(setup :mini.visits)
(setup :mini.extra)
(setup :mini.diff {:view {:style :sign :signs {:add :┃ :change :┃ :delete :▁}}
                   :delay {:text_change 50}}
  (keymap :n "yoD" MiniDiff.toggle_overlay)
  (keymap :n "[c" #(if vim.wo.diff "[c" (do (vim.schedule #(MiniDiff.goto_hunk :prev)) "<Ignore>")) {:expr true})
  (keymap :n "]c" #(if vim.wo.diff "]c" (do (vim.schedule #(MiniDiff.goto_hunk :next)) "<Ignore>")) {:expr true}))

(autocmd mini# :LspAttach {:once true}
  #(setup :mini.notify {:content {:format #(. $ :msg)}
                        :window {:config #{:anchor "SE"
                                           :border :rounded
                                           :row (- vim.o.lines vim.o.cmdheight (math.min 1 vim.o.laststatus))}
                                          :winblend 0}}))

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
      (autocmd :LspAttach "*" #(keymap :n "<Space>s" #(MiniExtra.pickers.lsp {:scope :workspace_symbol}) {:buffer (. $1 :buf)}))
      (autocmd :LspDetach "*" #(vim.keymap.del :n "<Space>s" {:buffer (. $1 :buf)})))))

