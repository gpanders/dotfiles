(macro deftoggle [key option]
  `(keymap :n ,(.. :yo key) ,(: "<Cmd>setlocal %s!<Bar>set %s?<CR>" :format option option) {:desc ,(: "Toggle '%s'" :format option)}))

(deftoggle :<Bar> :cursorcolumn)
(deftoggle :c :cursorline)
(deftoggle :f :foldenable)
(deftoggle :n :number)
(deftoggle :r :relativenumber)
(deftoggle :s :spell)
(deftoggle :t :expandtab)
(deftoggle :w :wrap)
(keymap :n :yod #(.. :<Cmd> (if vim.o.diff :diffoff :diffthis) :<CR>) {:expr true :desc "Toggle 'diff'"})
