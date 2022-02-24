(fn load []
  "This function is called when dap is first loaded"
  (with-module [dap :dap]
    (let [widgets (require "dap.ui.widgets")
          scopes (widgets.sidebar widgets.scopes)]
      (fn dap.listeners.after.event_initialized.me []
        (set vim.b.dap true)
        (keymap :n "K" #((. (require "dap.ui.widgets") :hover)))
        (keymap :n "dq" dap.close)
        (keymap :n "d[f" dap.up)
        (keymap :n "d]f" dap.down)
        (keymap :n "drc" dap.run_to_cursor)
        (keymap :n "<C-N>" dap.step_over)
        (keymap :n "<C-P>" dap.step_back)
        (keymap :n "<C-S>" dap.step_into)
        (scopes.open)
        (dap.repl.open))
      (fn dap.listeners.after.event_terminated.me []
        (set vim.b.dap false)
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

    (macro sign [name text hl]
      `(vim.fn.sign_define ,name {:text ,text :texthl ,hl}))

    (sign :DapBreakpoint "●" :debugBreakpoint)
    (sign :DapBreakpointCondition "◐" :debugBreakpoint)
    (sign :DapBreakpointRejected "X" :debugBreakpoint)
    (sign :DapStopped "→" :debugPC)

    (set dap.adapters.lldb {:type :executable
                            :command "lldb-vscode"
                            :name "lldb"})
    (let [conf [{:name "Launch"
                 :type :lldb
                 :request :launch
                 :program #(vim.fn.input "Path to executable: " (.. (vim.loop.cwd) "/") :file)
                 :cwd "${workspaceFolder}"
                 :stopOnEntry false
                 :args #(vim.split (vim.fn.input "Args: ") " ")}
                {:name "Attach to process"
                 :type :lldb
                 :request :attach
                 :pid (. (require "dap.utils") :pick_process)
                 :args []}]]
      (set dap.configurations.cpp conf)
      (set dap.configurations.c conf)
      (set dap.configurations.rust conf)
      (set dap.configurations.zig conf))))

(tset package.loaded :dap (setmetatable {} {:__index (fn [t k]
                                                       (tset package.loaded :dap nil)
                                                       (load)
                                                       (. (require :dap) k))}))

(let [dap (lazy-require :dap)]
  (keymap :n "dgb" #(dap.toggle_breakpoint))
  (keymap :n "drr" #(dap.continue))
  (keymap :n "drl" #(dap.run_last)))
