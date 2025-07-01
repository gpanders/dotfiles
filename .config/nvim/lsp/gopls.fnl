{:filetypes ["go" "gomod" "gowork" "gosum"]
 :cmd ["gopls"]
 :root_dir (fn [buf cb]
             (let [cwd (vim.fs.dirname (vim.api.nvim_buf_get_name buf))]
               (when (not (pcall vim.system ["go" "env" "-json" "GOMOD"]
                                            {: cwd}
                                            (fn [out]
                                              (when (= 0 out.code)
                                                (case (pcall vim.json.decode out.stdout)
                                                  (true {: GOMOD}) (when (not= GOMOD "/dev/null")
                                                                     (cb (vim.fs.dirname GOMOD))))))))
                 (case (vim.fs.root buf ["go.mod"])
                   root (case (vim.fs.root root ["go.work"])
                          workspace (cb workspace)
                          _ (cb root))))))
 :settings {:autoformat true
            :gopls {:analyses {:unusedparams true
                               :unusedwrite true
                               :nilness true}
                    :gofumpt true
                    :semanticTokens true
                    :staticcheck true
                    :directoryFilters ["-bazel-bin" "-bazel-out"]}}}
