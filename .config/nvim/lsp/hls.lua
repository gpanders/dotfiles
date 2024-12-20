return {
    filetypes = { "haskell" },
    cmd = { "haskell-language-server-wrapper", "--lsp" },
    name = "hls",
    root_markers = { "*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml" },
    settings = {
        haskell = {
            formattingProvider = "ormolu",
        },
    },
}
