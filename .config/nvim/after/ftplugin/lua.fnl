(vim.cmd.setlocal "comments^=:---")

(let [lsp (require :lsp)]
  (lsp.start {:cmd ["lua-language-server"]
              :root [".luarc.json"]
              :settings {:Lua {:telemetry {:enable false}}}}))
