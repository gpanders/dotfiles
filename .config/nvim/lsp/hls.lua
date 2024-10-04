return {
    filetype = "haskell",
    cmd = { "haskell-language-server-wrapper", "--lsp" },
    name = "hls",
    root_dir = vim.fs.root(0, { "*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml" }),
    settings = {
        haskell = {
            formattingProvider = "ormolu",
        },
    },
}
