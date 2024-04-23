return {
    filetype = "haskell",
    cmd = { "haskell-language-server-wrapper", "--lsp" },
    name = "hls",
    root = { "*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml" },
    settings = {
        haskell = {
            formattingProvider = "ormolu",
        },
    },
}
