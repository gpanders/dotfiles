local cachedir = vim.fn.stdpath("cache")
local confdir = vim.fn.stdpath("config")

local log = (function()
    local logfile = assert(io.open(cachedir .. "/fennel.log", "a"))
    return function(msg)
        logfile:write(string.format("[%s] %s\n", os.date("%F %T"), msg))
        logfile:flush()
    end
end)()

local fennel = setmetatable({}, {
    __index = function(_, k)
        local ok, fennel = pcall(require, "fennel")
        if not ok then
            local install_path = vim.fn.stdpath("data") .. "/site/pack/fennel/start/fennel"
            local tag = "0.10.0"
            print("Installing fennel " .. tag .. " to " .. install_path .. "...")
            vim.fn.system({"git", "clone", "-b", tag, "https://git.sr.ht/~technomancy/fennel", install_path})
            vim.fn.system({"make", "-C", install_path})
            vim.fn.system({"mkdir", install_path .. "/lua"})
            vim.fn.system({"mv", install_path .. "/fennel.lua", install_path .. "/lua"})
            vim.api.nvim_command("redraw")
            fennel = require("fennel")
        end
        return fennel[k]
    end,
})

local function compile(path)
    local luapath = path:gsub("^" .. confdir, cachedir .. "/fennel"):gsub("%.fnl$", ".lua")
    local s = vim.loop.fs_stat(luapath)
    if not s or s.mtime.sec < vim.loop.fs_stat(path).mtime.sec then
        local src = assert(io.open(path))
        local input = src:read("*a")
        src:close()

        local ok, output = pcall(fennel.compileString, input)
        if not ok then
            local msg = string.format("%s: %s", path, output)
            log(msg)
            error(msg, 0)
        end

        vim.fn.mkdir(luapath:match("(.+)/.-%.lua"), "p")
        local dst = assert(io.open(luapath, "w"))
        dst:write(output)
        dst:close()
        log(string.format("Compiled %s to %s", path, luapath))
    end
    return luapath
end

local function loader(name)
    local basename = name:gsub("%.", "/")
    local paths = { "fnl/" .. basename .. ".fnl", "fnl/" .. basename .. "/init.fnl" }
    for _, path in ipairs(paths) do
        local found = vim.api.nvim_get_runtime_file(path, false)
        if #found > 0 then
            local luafile = compile(found[1])
            local f, err = loadfile(luafile)
            return f or error(err)
        end
    end
end

table.insert(package.loaders, loader)
