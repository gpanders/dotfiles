local ignored = {
    ["fnl/macros.fnl"] = true,
}

local function compile(path)
    local base = path:gsub(vim.fn.stdpath("config") .. "/", "")
    if ignored[base] then
        return
    end

    local fennel_version = "1.3.0"
    local ok, fennel = pcall(require, "fennel")
    if not ok or fennel.version ~= fennel_version then
        local url = string.format("https://fennel-lang.org/downloads/fennel-%s.tar.gz", fennel_version)
        local tmpdir = vim.fn.fnamemodify(vim.fn.tempname(), ":h")
        vim.fn.mkdir(tmpdir, "p")
        print(string.format("Downloading Fennel %s...", fennel_version, tmpdir))
        local stderr
        local jobid = vim.fn.jobstart(string.format("curl -sS -o '%s/fennel.tar.gz' '%s'", tmpdir, url), {
            stderr_buffered = true,
            on_stderr = function(_, data)
                stderr = table.concat(data, "\n")
            end,
        })

        local success = vim.wait(10000, function()
            return vim.fn.jobwait({ jobid }, 0)[1] ~= -1
        end)

        assert(success, stderr)
        local out = vim.fn.system(string.format("tar -C %s -xf %s/fennel.tar.gz", tmpdir, tmpdir))
        assert(vim.v.shell_error == 0, out)

        local fennel_path = vim.fn.stdpath("data") .. "/site/lua/fennel.lua"
        vim.fn.mkdir(vim.fn.fnamemodify(fennel_path, ":h"), "p")
        out = vim.fn.system(string.format("mv %s/fennel-%s/fennel.lua %s", tmpdir, fennel_version, fennel_path))
        assert(vim.v.shell_error == 0, out)

        -- Clear all cached modules from an existing fennel installation.
        -- Without this, dofile() below will simply reload the existing version
        -- of fennel instead of picking up the new one
        for _, v in ipairs({"loaded", "preload"}) do
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
    local outf = io.open(out, "w")
    outf:write(compiled)
    outf:close()

    return out
end

local function walk(dir, ext, f)
    local subdirs = { "plugin", "indent", "ftplugin", "colors" }
    table.insert(subdirs, ext)
    for _, path in ipairs({ dir, dir .. "/after" }) do
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

return {
    compile = compile,
    walk = walk,
}
