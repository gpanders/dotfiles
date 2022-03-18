(keymap :n :q "<C-W>q" {:buffer true :nowait true})
(keymap :n :d "<C-D>" {:buffer true :nowait true})
(keymap :n :u "<C-U>" {:buffer true})
(keymap :n :f "<C-F>" {:buffer true :nowait true})
(keymap :n :b "<C-B>" {:buffer true})
(keymap :n "<Tab>" "/\\C\\%>1l\\f\\+([1-9][a-z]\\=)\\ze\\_.\\+\\%$<CR><Cmd>nohlsearch<CR>" {:buffer true})
(keymap :n "<S-Tab>" "?\\C\\%>1l\\f\\+([1-9][a-z]\\=)\\ze\\_.\\+\\%$<CR><Cmd>nohlsearch<CR>" {:buffer true})
(keymap :n "<CR>" "<C-]>" {:buffer true})
(keymap :n "<BS>" "<C-T>" {:buffer true})

(let [name (string.match (nvim.buf.get_name 0) "man://(.+)")]
  (setlocal statusline (: " %s%%=%%14.(%%l:%%c%%V%%)%%14.P " :format name)))
(append! vim.b.undo_ftplugin
         (accumulate [undo "" _ key (ipairs [:q :d :u :f :b "<Tab>" "<S-Tab>" "<CR>" "<BS>"])]
           (.. undo (: "|nun <buffer> %s" :format key))))

(append! vim.b.undo_ftplugin "|setl stl<")
