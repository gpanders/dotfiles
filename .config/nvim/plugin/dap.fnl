(autocmd dap# :FileType "c,cpp,rust,python" :once
  (with-module [dap :dap]
    (with-module [dap-python :dap-python]
      (dap-python.setup "~/.local/share/venv/debugpy/bin/python"))

    (set dap.adapters.lldb {:type :executable :command :lldb-vscode :name :lldb})
    (set dap.configurations.cpp [{:name :Launch
                                  :type :lldb
                                  :request :launch
                                  :program #(vim.fn.input "Path to executable: " (.. (vim.fn.getcwd) "/") :file)
                                  :cwd "${workspaceFolder}"
                                  :stopOnEntry false
                                  :args {}
                                  :runInTerminal false}])
    (set dap.configurations.c dap.configurations.cpp)
    (set dap.configurations.rust dap.configurations.cpp)

    (each [_ name (ipairs [:Breakpoint :BreakpointCondition :LogPoint :Stopped :BreakpointRejected])]
      (let [name (.. "Dap" name)]
        (vim.fn.sign_define name {:texthl name})))

    (keymap :n "<F5>" "<Cmd>lua require('dap').continue()<CR>")
    (keymap :n "<F10>" "<Cmd>lua require('dap').step_over()<CR>")
    (keymap :n "<F11>" "<Cmd>lua require('dap').step_into()<CR>")
    (keymap :n "<F12>" "<Cmd>lua require('dap').step_out()<CR>")
    (keymap :n "<Bslash>b" "<Cmd>lua require('dap').toggle_breakpoint()<CR>")
    (keymap :n "<Bslash>B" "<Cmd>lua require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>")
    (keymap :n "<Bslash>dr" "<Cmd>lua require('dap').repl.open()<CR>")
    (keymap :n "<Bslash>dl" "<Cmd>lua require('dap').run_last()<CR>")))
