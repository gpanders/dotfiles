(fn make [{: args}]
  (let [makeprg (case (vim.o.makeprg:gsub "%$%*" args)
                 (s 0) (.. s " " args)
                 (s n) s)
        [cmd & args] (vim.split makeprg " " {:trimempty true})
        stdout (vim.loop.new_pipe false)
        stderr (vim.loop.new_pipe false)
        chunks []
        qf-id (do
                (vim.fn.setqflist [] " " {:title makeprg :nr "$"})
                (let [{: id} (vim.fn.getqflist {:id 0})]
                  id))]
   (vim.cmd "copen|wincmd p")
   (nvim.exec_autocmds :QuickFixCmdPre {:pattern "make" :modeline false})
   (var handle nil)
   (fn on-exit [code]
     (when handle
       (handle:close))
     (vim.schedule #(do
                      (nvim.exec_autocmds :QuickFixCmdPost {:pattern "make" :modeline false})
                      (when (= code 0)
                        (vim.cmd.cclose)))))
   (set handle (vim.loop.spawn cmd {: args :stdio [nil stdout stderr]} on-exit))
   (fn on-data [err data]
     (assert (not err) err)
     (when data
       (vim.schedule #(let [lines (vim.split data "\n" {:trimempty true})]
                        (vim.fn.setqflist [] :a {:id qf-id : lines})
                        (vim.cmd.cbottom)))))
   (stdout:read_start on-data)
   (stderr:read_start on-data)))

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
