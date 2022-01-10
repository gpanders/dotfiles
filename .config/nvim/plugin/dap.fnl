(with-module [dap :dap]
  (set dap.adapters.lldb {:type :executable
                          :command "lldb-vscode"
                          :name "lldb"})
  (set dap.configurations.cpp [{:name "Launch"
                                :type :lldb
                                :request :launch
                                :program #(vim.fn.input "Path to executable: " (.. (vim.loop.cwd) "/") :file)
                                :cwd "${workspaceFolder}"
                                :stopOnEntry false
                                :args []}
                               {:name "Attach to process"
                                :type :lldb
                                :request :attach
                                :pid (. (require "dap.utils") :pick_process)
                                :args []}])
  (set dap.configurations.c dap.configurations.cpp)
  (set dap.configurations.rust dap.configurations.cpp)
  (set dap.configurations.zig dap.configurations.cpp)

  (let [widgets (require "dap.ui.widgets")
        scopes (widgets.sidebar widgets.scopes)]
    (fn dap.listeners.after.event_initialized.me []
      (keymap :n "K" #((. (require "dap.ui.widgets") :hover)))
      (scopes.open)
      (dap.repl.open))
    (fn dap.listeners.after.event_terminated.me []
      (vim.api.nvim_del_keymap :n "K")
      (scopes.close)
      (dap.repl.close)))

  (keymap :n "<Bslash>b" #(dap.toggle_breakpoint))
  (keymap :n "<Bslash>c" #(dap.continue))
  (keymap :n "<Bslash>n" #(dap.step_over))
  (keymap :n "<Bslash>s" #(dap.step_into))
  (keymap :n "<Bslash>d" #(dap.repl.open))

  (macro sign [name text hl]
    `(vim.fn.sign_define ,name {:text ,text :texthl ,hl}))

  (sign :DapBreakpoint "●" :debugBreakpoint)
  (sign :DapBreakpointCondition "◐" :debugBreakpoint)
  (sign :DapBreakpointRejected "X" :debugBreakpoint)
  (sign :DapStopped "→" :debugPC))
