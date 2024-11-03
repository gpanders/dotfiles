return {
    filetype = { "go", "gomod" },
    cmd = { "gopls" },
    root_dir = vim.fs.root(0, { "go.work", "go.mod" }),
    settings = {
        autoformat = true,
        gopls = {
            analyses = {
                unusedparams = true,
                unusedwrite = true,
                nilness = true,
            },
        },
    },
}
