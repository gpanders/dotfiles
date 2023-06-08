(fn grep [{: args}]
  (let [grepcmd (case (vim.o.grepprg:gsub "%$%*" args)
                  (s 0) (.. s " " args)
                  (s n) s)]
    (nvim.exec_autocmds :QuickFixCmdPre {:pattern "grep" :modeline false})
    (fn callback [{: code : stdout : stderr}]
      (let [src (if (= 0 code) stdout stderr)
            lines (vim.split src "\n" {:tripempty true})]
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
     (vim.cmd (.. "cclose|botright " (math.min 10 (length list)) "cwindow"))))

(vim.cmd "
cnoreabbrev <expr> gr    (getcmdtype() ==# ':' && getcmdline() ==# 'gr')    ? 'Grep'  : 'gr'
cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() ==# 'grep')  ? 'Grep'  : 'grep'
")

(keymap :n "<Space>/" ":Grep " {:silent false})
(keymap :x "<Space>/" "y:<C-U>Grep <C-R>\"" {:silent false})
(keymap :n "<Space>*" ":Grep <C-R><C-W><CR>")
(keymap :x "<Space>*" "y:<C-U>Grep <C-R>\"<CR>")
