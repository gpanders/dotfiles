return {
    filetype = "rust",
    cmd = { "rust-analyzer" },
    root_dir = vim.fs.root(0, { "Cargo.toml" }),
    settings = {
        autoformat = true,
        ["rust-analyzer"] = {
            check = {
                command = "clippy",
            },
        },
    },
}
