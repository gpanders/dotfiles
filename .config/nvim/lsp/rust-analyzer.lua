return {
    filetype = "rust",
    cmd = { "rust-analyzer" },
    root = { "Cargo.toml" },
    settings = {
        ["rust-analyzer"] = {
            check = {
                command = "clippy",
            },
        },
    },
}
