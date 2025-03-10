{:filetypes ["go" "gomod" "gowork" "gosum"]
 :cmd ["gopls"]
 :root_dir (fn [buf cb]
             (case (vim.fs.root buf ["go.mod"])
               root (case (vim.fs.root root ["go.work"])
                      workspace (cb workspace)
                      _ (cb root))
               _ (cb nil)))
 :settings {:autoformat true
            :gopls {:analyses {:unusedparams true
                               :unusedwrite true
                               :nilness true}}}}
