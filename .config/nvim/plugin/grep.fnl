(fn on-exit [l title mods chunks]
  (let [lines (-> chunks table.concat (vim.split "\n" {:trimempty true}))
        items (icollect [_ line (ipairs lines)]
                (let [(filename lnum col text) (line:match "^([^:]+):(%d+):(%d+):(.*)$")]
                  {: filename : lnum : col : text}))
        what {: title : items :nr "$"}]
    (if l
        (vim.fn.setloclist 0 {} " " what)
        (vim.fn.setqflist {} " " what)))
  (exec (.. mods " " (if l "lopen" "copen")))
  (exec (.. "doautocmd QuickFixCmdPost " (if l "lgrep" "grep"))))

(fn grep [l {: mods : args}]
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

(command :Grep {:nargs :+ :complete :file_in_path} (partial grep false))
(command :LGrep {:nargs :+ :complete :file_in_path} (partial grep true))

(vim.cmd "
cnoreabbrev <expr> gr    (getcmdtype() ==# ':' && getcmdline() ==# 'gr')    ? 'Grep'  : 'gr'
cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() ==# 'grep')  ? 'Grep'  : 'grep'
cnoreabbrev <expr> lgr   (getcmdtype() ==# ':' && getcmdline() ==# 'lgr')   ? 'LGrep' : 'lgr'
cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() ==# 'lgrep') ? 'LGrep' : 'lgrep'
")

(keymap :n "g/" ":Grep " {:silent false})
(keymap :x "g/" "y:<C-U>Grep <C-R>\"" {:silent false})
