(fn grep [l mods args]
  (local chunks [])
  (let [args (vim.fn.expandcmd args)
        grepcmd (match (vim.o.grepprg:gsub "%$%*" args)
                  (s 0) (.. s " " args)
                  s s)
        stdout (vim.loop.new_pipe false)]
    (fn on-exit []
      (let [lines (vim.split (vim.trim (table.concat chunks)) "\n")
            what {:title grepcmd :efm vim.o.grepformat :nr "$" : lines}]
        (if l
            (vim.fn.setloclist 0 {} " " what)
            (vim.fn.setqflist {} " " what)))
      (exec (.. mods " " (or (and l "lopen") "copen")))
      (exec (.. "doautocmd QuickFixCmdPost " (or (and l "lgrep") "grep"))))

    (exec (.. "doautocmd QuickFixCmdPre " (or (and l "lgrep") "grep")))
    (vim.loop.spawn
      vim.o.shell
      {:args ["-c" grepcmd] :stdio [nil stdout nil]}
      (vim.schedule_wrap on-exit))
    (stdout:read_start
      (fn [err data]
        (assert (not err) err)
        (when data
          (table.insert chunks data))))
    (print grepcmd)))

(command :Grep {:nargs :+ :complete :file_in_path} #(grep false $2 $3))
(command :LGrep {:nargs :+ :complete :file_in_path} #(grep true $2 $3))

(exec "cnoreabbrev <expr> gr    (getcmdtype() ==# ':' && getcmdline() ==# 'gr')    ? 'Grep'  : 'gr'")
(exec "cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() ==# 'grep')  ? 'Grep'  : 'grep'")
(exec "cnoreabbrev <expr> lgr   (getcmdtype() ==# ':' && getcmdline() ==# 'lgr')   ? 'LGrep' : 'lgr'")
(exec "cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() ==# 'lgrep') ? 'LGrep' : 'lgrep'")
