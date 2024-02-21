(fn grep [{: args}]
  (let [grepcmd (case (vim.o.grepprg:gsub "%$%*" args)
                  (s 0) (.. s " " args)
                  (s n) s)]
    (nvim.exec_autocmds :QuickFixCmdPre {:pattern "grep" :modeline false})
    (fn callback [{: code : stdout : stderr}]
      (let [src (if (= 0 code) stdout stderr)
            lines (vim.split src "\n" {:trimempty true})]
        (vim.schedule #(do
                         (vim.fn.setqflist [] " " {:title grepcmd
                                                   : lines
                                                   :efm vim.o.grepformat
                                                   :nr "$"})
                         (nvim.exec_autocmds :QuickFixCmdPost {:pattern "grep" :modeline false})))))
    (vim.system [vim.o.shell "-c" grepcmd] callback)
    (print grepcmd)))

(command :Grep {:nargs :+ :complete :file_in_path} grep)

(autocmd grep# :QuickFixCmdPost :grep {:nested true}
  #(let [list (vim.fn.getqflist)]
     (exec (.. "cclose|botright " (math.min 10 (length list)) "cwindow"))))

(keymap :ca "gr" #(if (and (= ":" (vim.fn.getcmdtype)) (= "gr" (vim.fn.getcmdline))) :Grep :gr) {:expr true :silent false})
(keymap :ca "grep" #(if (and (= ":" (vim.fn.getcmdtype)) (= "grep" (vim.fn.getcmdline))) :Grep :grep) {:expr true :silent false})
(keymap :n "<Space>/" ":Grep " {:silent false})
(keymap :x "<Space>/" "y:<C-U>Grep <C-R>\"" {:silent false})
(keymap :n "<Space>*" ":Grep <C-R><C-W><CR>")
(keymap :x "<Space>*" "y:<C-U>Grep <C-R>\"<CR>")
