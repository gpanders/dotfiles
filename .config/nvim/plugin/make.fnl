(fn create-qf [title handle]
  (nvim.exec_autocmds :QuickFixCmdPre {:pattern "make" :modeline false})
  (vim.fn.setqflist [] " " {: title :nr "$"})
  (vim.cmd "botright copen|wincmd p")
  (let [{: id : qfbufnr} (vim.fn.getqflist {:id 0 :qfbufnr true :winid true})]
    (keymap :n "<C-C>" #(let [title (: "%s (Interrupted)" :format title)]
                          (handle:kill vim.loop.constants.SIGINT)
                          (vim.fn.setqflist [] :a {: title})) {:buffer qfbufnr})
    id))

(fn make [{: args}]
  (let [makeprg (case (vim.o.makeprg:gsub "%$%*" args)
                  (s 0) (.. s " " args)
                  (s n) s)
        makeprg (vim.fn.expandcmd (vim.trim makeprg))
        [cmd & args] (vim.split makeprg " ")
        stdout (vim.loop.new_pipe false)
        stderr (vim.loop.new_pipe false)]
   (var handle nil)
   (fn on-exit [code]
     (when handle
       (handle:close))
     (stdout:close)
     (stderr:close)
     (vim.schedule #(do
                      (nvim.exec_autocmds :QuickFixCmdPost {:pattern "make" :modeline false})
                      (if (= code 0)
                          (vim.cmd.cclose)
                          (print (: "Command %s exited with error code %d" :format makeprg code))))))
   (match (vim.loop.spawn cmd {: args :stdio [nil stdout stderr]} on-exit)
     (nil err) (print (: "Failed to spawn process: %s" :format err))
     h (set handle h))
   (when handle
     (var qf nil)
     (fn on-data [err data]
       (assert (not err) err)
       (when data
         (vim.schedule #(let [lines (vim.split data "\n" {:trimempty true})]
                          (when (not qf)
                            (set qf (create-qf makeprg handle)))
                          (vim.fn.setqflist [] :a {:id qf : lines})
                          (vim.cmd.cbottom)))))
     (stdout:read_start on-data)
     (stderr:read_start on-data))))

(command :Make {:nargs :*} make)

(autocmd make# :QuickFixCmdPost :make {:nested true}
  "Focus the quickfix window on the first error (if any)"
  #(let [{: items : winid} (vim.fn.getqflist {:items true :winid true})]
     (var done? false)
     (each [i item (ipairs items) &until done?]
       (when (= 1 item.valid)
         (set done? true)
         (nvim.win_set_cursor winid [i 0])))))

(keymap :n "m?" #(print vim.o.makeprg))
(keymap :n "m<Space>" ":<C-U>Make " {:silent false})
(keymap :n "m<CR>" "<Cmd>Make<CR>")
