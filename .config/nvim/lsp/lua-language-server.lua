return {
    filetype = "lua",
    cmd = { "lua-language-server" },
    root = { ".luarc.json" },
    settings = {
        Lua = {
            telemetry = {
                enable = false,
            },
        },
    },
}
