(with-module [dap :dap]
  (keymap :n "<F5>" #(dap.continue))
  (keymap :n "<F6>" #(dap.run_to_cursor))
  (keymap :n "<F10>" #(dap.step_over))
  (keymap :n "<F11>" #(dap.step_into))
  (keymap :n "<F12>" #(dap.step_out))

  (keymap :n "<Space>db" #(dap.toggle_breakpoint))
  (keymap :n "<Space>dr" #(dap.repl.open {:height 25}))

  (keymap :n "<Space>dh" #((. (require :dap.ui.widgets) :hover)))
  (keymap :n "<Space>df" #(let [{: centered_float : frames} (require :dap.ui.widgets)]
                            (centered_float frames)))
  (keymap :n "<Space>ds" #(let [{: centered_float : scopes} (require :dap.ui.widgets)]
                            (centered_float scopes)))

  (vim.fn.sign_define :DapStopped {:text "→" :linehl "DapStoppedLine" :texthl "DapStoppedSign"})
  (vim.fn.sign_define :DapBreakpoint {:text "●" :texthl "DapBreakpoint"})
  (vim.fn.sign_define :DapBreakpointRejected {:text "○" :texthl "DapBreakpoint"})

  (fn dap.listeners.after.event_initialized.me [session event]
    (let [win (nvim.get_current_win)
          widgets (require :dap.ui.widgets)
          frames (widgets.sidebar widgets.frames {:wrap false} "topleft 60vsplit")
          scopes (widgets.sidebar widgets.scopes nil "aboveleft new")]
      (frames.open)

      ; Holy crap this is hacky. There has to be a better way
      (nvim.set_current_win frames.win)
      (scopes.open)
      (vim.cmd.wincmd :p)
      (nvim.win_set_buf 0 scopes.buf)
      (nvim.set_current_win win)))

  (fn dap.listeners.after.event_stopped.me []
    (keymap :n "<M-S-J>" #(dap.down))
    (keymap :n "<M-S-K>" #(dap.up)))

  (fn dap.listeners.after.event_continued.me []
    (nvim.del_keymap :n "<M-S-J>")
    (nvim.del_keymap :n "<M-S-K>")))
