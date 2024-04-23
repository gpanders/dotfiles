return {
    filetype = "go",
    cmd = { "gopls" },
    root = { "go.mod" },
    settings = {
        gopls = {
            analyses = {
                unusedparams = true,
                unusedwrite = true,
                nilness = true,
            },
        },
    },
}
