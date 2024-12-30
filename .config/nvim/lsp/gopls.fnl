{:filetypes ["go" "gomod" "gowork" "gosum"]
 :cmd ["gopls"]
 :root_dir (fn [cb]
             (case (vim.fs.root 0 ["go.mod"])
               root (case (vim.fs.root root ["go.work"])
                      workspace (cb workspace)
                      _ (cb root))
               _ (cb nil)))
 :settings {:autoformat true
            :gopls {:analyses {:unusedparams true
                               :unusedwrite true
                               :nilness true}}}}
