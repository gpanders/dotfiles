(let [lsp (require :lsp)]
  (lsp.setup {:cmd ["tflint" "--langserver"]}))
