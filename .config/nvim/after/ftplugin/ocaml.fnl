(let [lsp (require :lsp)]
  (lsp.start {:cmd ["ocamllsp"]
              :root ["*.opam" "dune-project" "dune-workspace"]}))
