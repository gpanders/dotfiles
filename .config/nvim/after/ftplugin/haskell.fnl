(let [lsp (require :lsp)]
  (lsp.start {:cmd ["haskell-language-server-wrapper" "--lsp"]
              :name :hls
              :root ["*.cabal" "stack.yaml" "cabal.project" "package.yaml" "hie.yaml"]
              :settings {:haskell {:formattingProvider :ormolu}}}))
