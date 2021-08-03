(fn grep [l args mods]
  (local chunks [])

  (fn cb []
    (let [lines (vim.split (vim.trim (table.concat chunks)) "\n")
          what {:title grepcmd :efm vim.o.grepformat :nr "$" : lines}]
      (if l
          (vim.fn.setloclist 0 {} " " what)
          (vim.fn.setqflist {} " " what)))
    (vim.api.nvim_command (.. mods " " (or (and l :lopen) :copen)))
    (vim.api.nvim_command (.. "doautocmd QuickFixCmdPost "
                              (or (and l :lgrep) :grep))))

  (vim.api.nvim_command (.. "doautocmd QuickFixCmdPre "
                            (or (and l :lgrep) :grep)))
  (let [args (vim.fn.expandcmd args)
        grepcmd (if (string.match vim.o.grepprg "%$%*")
                    (string.gsub vim.o.grepprg "%$%*" args)
                    (.. vim.o.grepprg " " args))
        [cmd & args] (vim.split grepcmd " ")
        stdout (vim.loop.new_pipe false)]
    (vim.loop.spawn cmd {: args :stdio [nil stdout nil]} (vim.schedule_wrap cb))
    (stdout:read_start
      (fn [err data]
        (assert (not err) err)
        (when data
          (table.insert chunks data)))))
  (print grepcmd))
