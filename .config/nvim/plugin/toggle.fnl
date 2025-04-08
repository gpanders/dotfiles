(macro deftoggle [key option]
  `(do
     (keymap :n ,(.. "yo" key) ,(: "<Cmd>setlocal %s!<Bar>set %s?<CR>" :format option option) {:desc ,(: "Toggle '%s'" :format option)})
     (keymap :n ,(.. "[o" key) ,(: "<Cmd>setlocal %s<Bar>set %s?<CR>" :format option option) {:desc ,(: "Enable '%s'" :format option)})
     (keymap :n ,(.. "]o" key) ,(: "<Cmd>setlocal no%s<Bar>set %s?<CR>" :format option option) {:desc ,(: "Disable '%s'" :format option)})))

(deftoggle :<Bar> :cursorcolumn)
(deftoggle :c :cursorline)
(deftoggle :f :foldenable)
(deftoggle :n :number)
(deftoggle :r :relativenumber)
(deftoggle :s :spell)
(deftoggle :t :expandtab)
(deftoggle :w :wrap)
(keymap :n "yod" #(.. :<Cmd> (if vim.o.diff :diffoff :diffthis) :<CR>) {:expr true :desc "Toggle diff mode for the current window"})
(keymap :n "[od" "<Cmd>diffthis<CR>" {:desc "Enable diff mode for the current window"})
(keymap :n "]od" "<Cmd>diffoff<CR>" {:desc "Disable diff mode for the current window"})
