local ignored = {
    ["fnl/macros.fnl"] = true,
}

local fennel_version = "1.5.0"
local fennel_path = vim.fn.stdpath("data") .. "/site/lua/fennel.lua"
local fennel_url = string.format("https://fennel-lang.org/downloads/fennel-%s.lua", fennel_version)

local function compile(path)
    local base = path:gsub(vim.fn.stdpath("config") .. "/", "")
    if ignored[base] then
        return
    end

    local ok, fennel = pcall(require, "fennel")
    if not ok or fennel.version ~= fennel_version then
        print(string.format("Downloading Fennel %s...", fennel_version, tmpdir))

        vim.fn.mkdir(vim.fn.fnamemodify(fennel_path, ":h"), "p")
        do
            local job = vim.system({ "curl", "-sS", "-o", fennel_path, fennel_url }):wait(10000)
            assert(job.code == 0, job.stderr)
        end

        -- Clear all cached modules from an existing fennel installation.
        -- Without this, dofile() below will simply reload the existing version
        -- of fennel instead of picking up the new one
        for _, v in ipairs({ "loaded", "preload" }) do
            for k in pairs(package[v]) do
                if k:match("^fennel%.?") then
                    package[v][k] = nil
                end
            end
        end

        fennel = assert(dofile(fennel_path))
        package.loaded.fennel = fennel
    end

    local f = assert(io.open(path))
    local src = f:read("*a")
    f:close()

    if not src then
        return
    end

    local macro_path = fennel["macro-path"]
    fennel["macro-path"] = macro_path .. ";" .. vim.fn.stdpath("config") .. "/fnl/?.fnl"
    local preamble = [[(require-macros :macros)]]
    local compiled = fennel.compileString(preamble .. src, { filename = path })
    fennel["macro-path"] = macro_path

    local out = vim.fn.stdpath("data") .. "/site/" .. base:gsub("^fnl/", "lua/"):gsub("%.fnl$", ".lua")
    vim.fn.mkdir(vim.fn.fnamemodify(out, ":h"), "p")
    local outf = assert(io.open(out, "w"))
    outf:write(compiled)
    outf:close()

    return out
end

local function walk(dir, ext, f)
    local subdirs = { "plugin", "indent", "ftplugin", "colors", "lsp", ext }
    local pred = function(name)
        return name:match(string.format(".*%%.%s", ext))
    end
    for _, path in ipairs({ dir, dir .. "/after" }) do
        for _, subpath in ipairs(subdirs) do
            local files = vim.fs.find(pred, {
                limit = math.huge,
                path = vim.fs.joinpath(path, subpath),
            })
            for _, v in ipairs(files) do
                f(v)
            end
        end
    end
end

return {
    compile = compile,
    walk = walk,
}
