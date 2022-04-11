local ignored = {
    ["fnl/macros.fnl"] = true,
}

local function compile(path)
    local base = path:gsub(vim.fn.stdpath("config") .. "/", "")
    if ignored[base] then
        return
    end

    local ok, fennel = pcall(require, "fennel")
    if not ok then
        local version = "1.1.0"
        local url = string.format("https://fennel-lang.org/downloads/fennel-%s.lua", version)
        local fennel_path = vim.fn.stdpath("data") .. "/site/lua/fennel.lua"
        vim.fn.mkdir(vim.fn.fnamemodify(fennel_path, ":h"), "p")
        print(string.format("Downloading %s to %s...", url, fennel_path))
        local stderr
        local jobid = vim.fn.jobstart(string.format("curl -sS -o '%s' '%s'", fennel_path, url), {
            stderr_buffered = true,
            on_stderr = function(_, data)
                stderr = table.concat(data, "\n")
            end,
        })

        local success = vim.wait(10000, function()
            return vim.fn.jobwait({ jobid }, 0)[1] ~= -1
        end)

        assert(success, stderr)
        fennel = assert(dofile(fennel_path))
        package.loaded["fennel"] = fennel
    end

    local f = io.open(path)
    if not f then
        return
    end

    local src = f:read("*a")
    f:close()

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
