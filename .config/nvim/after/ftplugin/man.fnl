(keymap :n :q "<C-W>q" {:buffer true :nowait true})
(keymap :n :d "<C-D>" {:buffer true :nowait true})
(keymap :n :u "<C-U>" {:buffer true})
(keymap :n "<Tab>" "/\\C\\%>1l\\f\\+([1-9][a-z]\\=)\\ze\\_.\\+\\%$<CR><Cmd>nohlsearch<CR>" {:buffer true})
(keymap :n "<S-Tab>" "?\\C\\%>1l\\f\\+([1-9][a-z]\\=)\\ze\\_.\\+\\%$<CR><Cmd>nohlsearch<CR>" {:buffer true})
(keymap :n "<CR>" "<C-]>" {:buffer true})
(keymap :n "<BS>" "<C-T>" {:buffer true})

(append! vim.b.undo_ftplugin "|nun <buffer> q|nun <buffer> d|nun <buffer> u|nun <buffer> <Tab>|nun <buffer> <S-Tab>|nun <buffer> <CR>|nun <buffer> <BS>")
