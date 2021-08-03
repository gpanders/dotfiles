local confdir = vim.fn.stdpath("config")
local cachedir = vim.fn.stdpath("cache")

local logfile = assert(io.open(cachedir .. "/fennel.log", "a"))

local function log(msg)
    logfile:write(string.format("[%s] %s\n", os.date("%F %T"), msg))
    logfile:flush()
end

local function compile(path)
    local luapath = path:gsub("^" .. confdir, cachedir):gsub("%.fnl$", ".lua")
    local s = vim.loop.fs_stat(luapath)
    if not s or vim.loop.fs_stat(path).mtime.sec > s.mtime.sec then
        local src = assert(io.open(path))
        local compiled = require("fennel").compileString(src:read("*a"))
        src:close()
        vim.fn.mkdir(luapath:match("(.+)/.-%.lua"), "p")
        local f = assert(io.open(luapath, "w"))
        f:write(compiled)
        f:close()
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
