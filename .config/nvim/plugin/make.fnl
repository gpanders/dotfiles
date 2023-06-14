(fn make [{: args}]
  (let [makeprg (case (vim.o.makeprg:gsub "%$%*" args)
                  (s 0) (.. s " " args)
                  (s n) s)
        makeprg (vim.fn.expandcmd (vim.trim makeprg))
        state {}]
    (fn on-exit [{: code}]
      (vim.schedule (fn []
                      (vim.fn.setqflist [] :a {:id state.qf :context {: code}})
                      (nvim.exec_autocmds :QuickFixCmdPost {:pattern "make" :modeline false})
                      (when (not= code 0)
                        (print (: "Command %s exited with error code %d" :format makeprg code))))))
    (fn on-data [err data]
      (assert (not err) err)
      (when data
        (vim.schedule #(let [lines (vim.split data "\n" {:trimempty true})]
                         (when (not state.qf)
                           (vim.fn.setqflist [] " " {:title makeprg :nr "$"})
                           (vim.cmd "botright copen|wincmd p")
                           (let [{: id : qfbufnr} (vim.fn.getqflist {:id 0 :qfbufnr true})]
                             (set state.qf id)
                             (keymap :n "<C-C>" #(let [result (state.handle:wait 0)]
                                                   (when (not= 0 result.signal)
                                                     (let [title (: "%s (Interrupted)" :format makeprg)]
                                                       (vim.fn.setqflist [] :a {: title}))) {}) {:buffer qfbufnr})))
                         (vim.fn.setqflist [] :a {:id state.qf : lines})
                         (vim.cmd.cbottom)))))
    (nvim.exec_autocmds :QuickFixCmdPre {:pattern "make" :modeline false})
    (set state.handle (vim.system (vim.split makeprg " ") {:stdout on-data :stderr on-data} on-exit))))

(command :Make {:nargs :*} make)

(autocmd make# :QuickFixCmdPost :make {:nested true}
  "Focus the quickfix window on the first error (if any)"
  #(let [{: items : winid :context {: code}} (vim.fn.getqflist {:items true :winid true :context true})]
     (var found? false)
     (each [i item (ipairs items) &until found?]
       (when (= 1 item.valid)
         (set found? true)
         (nvim.win_set_cursor winid [i 0])))
     (when (and (= 0 code) (not found?))
       (vim.cmd.cclose))))

(keymap :n "m?" #(print vim.o.makeprg))
(keymap :n "m<Space>" ":<C-U>Make " {:silent false})
(keymap :n "m<CR>" "<Cmd>Make<CR>")
