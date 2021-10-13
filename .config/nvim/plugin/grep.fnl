(fn on-exit [l title mods chunks]
  (let [lines (-> (table.concat chunks) (vim.split "\n" {:trimempty true}))
        what {: title :efm vim.o.grepformat :nr "$" : lines}]
    (if l
        (vim.fn.setloclist 0 {} " " what)
        (vim.fn.setqflist {} " " what)))
  (exec (.. mods " " (if l "lopen" "copen")))
  (exec (.. "doautocmd QuickFixCmdPost " (if l "lgrep" "grep"))))

(fn grep [l mods args]
  (let [args (vim.fn.expandcmd args)
        mods (if (= mods "") :botright mods)
        grepcmd (match (vim.o.grepprg:gsub "%$%*" args)
                  (s n) (if (= n 0)
                            (.. s " " args)
                            s))
        stdout (vim.loop.new_pipe false)
        chunks []]
    (exec (.. "doautocmd QuickFixCmdPre " (or (and l "lgrep") "grep")))
    (vim.loop.spawn
      vim.o.shell
      {:args ["-c" grepcmd] :stdio [nil stdout nil]}
      (vim.schedule_wrap #(on-exit l grepcmd mods chunks)))
    (stdout:read_start
      (fn [err data]
        (assert (not err) err)
        (when data
          (table.insert chunks data))))
    (print grepcmd)))

(command :Grep {:nargs :+ :complete :file_in_path} #(grep false $2 $3))
(command :LGrep {:nargs :+ :complete :file_in_path} #(grep true $2 $3))

(vim.cmd "
cnoreabbrev <expr> gr    (getcmdtype() ==# ':' && getcmdline() ==# 'gr')    ? 'Grep'  : 'gr'
cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() ==# 'grep')  ? 'Grep'  : 'grep'
cnoreabbrev <expr> lgr   (getcmdtype() ==# ':' && getcmdline() ==# 'lgr')   ? 'LGrep' : 'lgr'
cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() ==# 'lgrep') ? 'LGrep' : 'lgrep'
")
