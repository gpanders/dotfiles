(local timer (vim.loop.new_timer))
(local timeout 250)

(vim.opt.eventignore:append [:CursorHold :CursorHoldI])

(fn callback [event]
  (vim.opt.eventignore:remove event)
  (exec (: "doautocmd <nomodeline> %s" :format event))
  (vim.opt.eventignore:append event))

(augroup cursorhold
  (autocmd :CursorMoved "*"
    (timer:stop)
    (let [{: mode} (nvim.get_mode)
          reg (vim.fn.reg_recording)]
      (when (and (= :n (mode:sub 1 1)) (= "" reg))
        (timer:start timeout 0 #(vim.schedule #(callback :CursorHold))))))
  (autocmd :CursorMovedI "*"
    (timer:stop)
    (let [reg (vim.fn.reg_recording)]
      (when (= "" reg)
        (timer:start timeout 0 #(vim.schedule #(callback :CursorHoldI)))))))
