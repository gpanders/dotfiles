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
                                :args #(vim.split (vim.fn.input "Args: ") " ")}
                               {:name "Attach to process"
                                :type :lldb
                                :request :attach
                                :pid (. (require "dap.utils") :pick_process)
                                :args []}])
  (set dap.configurations.c dap.configurations.cpp)
  (set dap.configurations.rust dap.configurations.cpp)
  (set dap.configurations.zig dap.configurations.cpp))
