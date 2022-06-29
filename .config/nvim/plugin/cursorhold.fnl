; Decouple CursorHold and CursorHoldI from 'updatetime' by firing them on timers
; See https://github.com/neovim/neovim/issues/12587

(local timer (vim.loop.new_timer))
(local timeout 250)

(autocmd cursorhold# :VimLeavePre #(timer:close))

(vim.opt.eventignore:append [:CursorHold :CursorHoldI])

(fn callback [event]
  (vim.opt.eventignore:remove event)
  (nvim.exec_autocmds event {:modeline false})
  (vim.opt.eventignore:append event))

(augroup cursorhold#
  (autocmd :CursorMoved "*"
    (fn []
      (timer:stop)
      (let [{: mode} (nvim.get_mode)
            reg (vim.fn.reg_recording)]
        (when (and (= :n (mode:sub 1 1)) (= "" reg))
          (timer:start timeout 0 #(vim.schedule #(callback :CursorHold)))))))
  (autocmd :CursorMovedI "*"
    (fn []
      (timer:stop)
      (let [reg (vim.fn.reg_recording)]
        (when (= "" reg)
          (timer:start timeout 0 #(vim.schedule #(callback :CursorHoldI))))))))
