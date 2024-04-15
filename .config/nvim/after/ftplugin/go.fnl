(set vim.bo.formatprg "gofmt")
(let [lsp (require :lsp)]
  (lsp.start {:cmd ["gopls"]
              :root ["go.mod"]
              :settings {:gopls {:analyses {:unusedparams true
                                            :unusedwrite true
                                            :nilness true}}}}))
