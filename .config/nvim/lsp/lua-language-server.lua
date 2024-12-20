return {
    filetypes = { "lua" },
    cmd = { "lua-language-server" },
    root_dir = vim.fs.root(0, { ".luarc.json" }),
    settings = {
        Lua = {
            telemetry = {
                enable = false,
            },
        },
    },
}
