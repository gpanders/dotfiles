local function compile()
    require("moonwalk").compile(vim.fn.expand("<afile>:p"))
end

vim.api.nvim_create_augroup("moonwalk", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", {
    pattern = vim.fn.stdpath("config") .. "/*.fnl",
    group = "moonwalk",
    callback = compile,
})
vim.api.nvim_create_autocmd("SourceCmd", {
    pattern = "*.fnl",
    group = "moonwalk",
    callback = compile,
})

local function moonwalk()
    local walk = require("moonwalk").walk

    -- Clean existing files
    walk(vim.fn.stdpath("data") .. "/site", "lua", function(path)
        if vim.fn.fnamemodify(path, ":t") == "fennel.lua" then
            return
        end
        os.remove(path)
    end)

    walk(vim.fn.stdpath("config"), "fnl", require("moonwalk").compile)
    local f = io.open(vim.fn.stdpath("cache") .. "/.compiled", "w")
    f:write("")
    f:close()
end

vim.api.nvim_add_user_command("Moonwalk", moonwalk, { desc = "Compile all Fennel runtime files to Lua" })

-- Using a filesystem marker to do this seems hacky, but I don't now of a better way
if not vim.loop.fs_stat(vim.fn.stdpath("cache") .. "/.compiled") then
    moonwalk()
end
