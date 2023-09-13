(let [lsp (require :lsp)]
  (lsp.start {:cmd ["tflint" "--langserver"]}))
