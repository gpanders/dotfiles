vim.api.nvim_create_augroup("moonwalk", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", {
    pattern = vim.fn.stdpath("config") .. "/*.fnl",
    group = "moonwalk",
    callback = function(args)
        require("moonwalk").compile(args.file)
    end,
})
vim.api.nvim_create_autocmd("SourceCmd", {
    pattern = "*.fnl",
    group = "moonwalk",
    callback = function(args)
        vim.api.nvim_command("source " .. require("moonwalk").compile(args.file))
    end,
})

local marker = vim.fn.stdpath("state") .. "/moonwalk"

local function moonwalk()
    vim.loader.enable(false)

    local walk = require("moonwalk").walk

    -- Clean existing files
    walk(vim.fn.stdpath("data") .. "/site", "lua", function(path)
        if vim.fn.fnamemodify(path, ":t") == "fennel.lua" then
            return
        end
        os.remove(path)
    end)

    walk(vim.fn.stdpath("config"), "fnl", require("moonwalk").compile)
    local f = assert(io.open(marker, "w"))
    f:write("")
    f:close()

    -- Reset runtimepath cache so new Lua files are discovered
    vim.o.runtimepath = vim.o.runtimepath

    vim.schedule(vim.loader.enable)
end

vim.api.nvim_create_user_command("Moonwalk", moonwalk, { desc = "Compile all Fennel runtime files to Lua" })

-- Using a filesystem marker to do this seems hacky, but I don't now of a better way
if not vim.uv.fs_stat(marker) then
    moonwalk()
end
