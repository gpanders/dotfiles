(with-module [dap :dap]
  (let [widgets (require "dap.ui.widgets")
        scopes (widgets.sidebar widgets.scopes)]
    (fn dap.listeners.after.event_initialized.me []
      (keymap :n "K" #((. (require "dap.ui.widgets") :hover)))
      (keymap :n "dq" #(dap.close))
      (keymap :n "d[f" #(dap.up))
      (keymap :n "d]f" #(dap.down))
      (keymap :n "drc" #(dap.run_to_cursor))
      (keymap :n "<C-N>" #(dap.step_over))
      (keymap :n "<C-P>" #(dap.step_back))
      (keymap :n "<C-S>" #(dap.step_into))
      (scopes.open)
      (dap.repl.open))
    (fn dap.listeners.after.event_terminated.me []
      (vim.api.nvim_del_keymap :n "K")
      (vim.api.nvim_del_keymap :n "dq")
      (vim.api.nvim_del_keymap :n "d[f")
      (vim.api.nvim_del_keymap :n "d]f")
      (vim.api.nvim_del_keymap :n "drc")
      (vim.api.nvim_del_keymap :n "<C-N>")
      (vim.api.nvim_del_keymap :n "<C-P>")
      (vim.api.nvim_del_keymap :n "<C-S>")
      (scopes.close)
      (dap.repl.close)))

  (keymap :n "dgb" #(dap.toggle_breakpoint))
  (keymap :n "drr" #(dap.continue))
  (keymap :n "drl" #(dap.run_last))

  (macro sign [name text hl]
    `(vim.fn.sign_define ,name {:text ,text :texthl ,hl}))

  (sign :DapBreakpoint "●" :debugBreakpoint)
  (sign :DapBreakpointCondition "◐" :debugBreakpoint)
  (sign :DapBreakpointRejected "X" :debugBreakpoint)
  (sign :DapStopped "→" :debugPC))
