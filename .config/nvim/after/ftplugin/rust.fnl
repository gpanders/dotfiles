(let [lsp (require :lsp)]
  (lsp.start {:cmd ["rust-analyzer"]
              :root ["Cargo.toml"]}))
