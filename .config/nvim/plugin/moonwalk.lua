vim.cmd(string.format([[
augroup fennel
  autocmd!
  autocmd BufWritePost %s/*.fnl lua require("moonwalk").compile(vim.fn.expand("<afile>:p"))
  autocmd SourceCmd *.fnl lua dofile(require("moonwalk").compile(vim.fn.expand("<afile>:p")))
augroup END]], vim.fn.stdpath("config")))

local function walk(dir, ext, f)
    local subdirs = {"plugin", "indent", "ftplugin", "colors"}
    table.insert(subdirs, ext)
    for _, path in ipairs({dir, dir .. "/after"}) do
        for _, subpath in ipairs(subdirs) do
            for _, v in ipairs(vim.fn.globpath(path, subpath .. "/*." .. ext, false, true)) do
                f(v)
            end
            for _, v in ipairs(vim.fn.globpath(path, subpath .. "/*/*." .. ext, false, true)) do
                f(v)
            end
        end
    end
end

vim.api.nvim_add_user_command("Moonwalk", function()
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
end, {
    desc = "Compile all Fennel runtime files to Lua",
})

-- Using a filesystem marker to do this seems hacky, but I don't now of a better way
if not vim.loop.fs_stat(vim.fn.stdpath("cache") .. "/.compiled") then
    vim.api.nvim_command("Moonwalk")
end
