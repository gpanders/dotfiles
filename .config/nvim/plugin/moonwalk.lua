vim.cmd(string.format([[
augroup fennel
  autocmd!
  autocmd BufWritePost %s/*.fnl lua require("moonwalk").compile(vim.fn.expand("<afile>:p"))
  autocmd SourceCmd *.fnl lua dofile(require("moonwalk").compile(vim.fn.expand("<afile>:p")))
augroup END]], vim.fn.stdpath("config")))

vim.api.nvim_add_user_command("Moonwalk", function()
    local compile = require("moonwalk").compile
    for _, path in ipairs({vim.fn.stdpath("config"), vim.fn.stdpath("config") .. "/after"}) do
        for _, subpath in ipairs({"plugin", "indent", "ftplugin", "fnl"}) do
            for _, v in ipairs(vim.fn.globpath(path, subpath .. "/*.fnl", false, true)) do
                compile(v)
            end
            for _, v in ipairs(vim.fn.globpath(path, subpath .. "/*/*.fnl", false, true)) do
                compile(v)
            end
        end
    end

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
