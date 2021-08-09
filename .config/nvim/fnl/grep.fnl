(import-macros {: exec} :macros)

(fn grep [l args mods]
  (local chunks [])

  (fn on-exit []
    (let [lines (vim.split (vim.trim (table.concat chunks)) "\n")
          what {:title grepcmd :efm vim.o.grepformat :nr "$" : lines}]
      (if l
          (vim.fn.setloclist 0 {} " " what)
          (vim.fn.setqflist {} " " what)))
    (exec (.. mods " " (or (and l "lopen") "copen")))
    (exec (.. "doautocmd QuickFixCmdPost " (or (and l "lgrep") "grep"))))

  (exec (.. "doautocmd QuickFixCmdPre " (or (and l "lgrep") "grep")))
  (let [args (vim.fn.expandcmd args)
        grepcmd (match (string.gsub vim.o.grepprg "%$%*" args)
                  (s 0) (.. s " " args)
                  s s)
        stdout (vim.loop.new_pipe false)]
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
