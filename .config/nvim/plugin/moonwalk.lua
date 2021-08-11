local ok, moonwalk = pcall(require, "moonwalk")
if not ok then
    return
end

local fennel = setmetatable({}, {
    __index = function(_, k)
        if not pcall(require, "fennel") then
            local install_path = vim.fn.stdpath("data") .. "/site/pack/fennel/start/fennel"
            local tag = "0.10.0"
            print("Installing fennel " .. tag .. " to " .. install_path .. "...")

            local out
            out = vim.fn.system({ "git", "clone", "-b", tag, "https://git.sr.ht/~technomancy/fennel", install_path })
            assert(vim.v.shell_error == 0, out)

            out = vim.fn.system({ "make", "-C", install_path })
            assert(vim.v.shell_error == 0, out)

            vim.fn.system({ "mkdir", install_path .. "/lua" })
            vim.fn.system({ "mv", install_path .. "/fennel.lua", install_path .. "/lua" })
            vim.api.nvim_command("redraw")
        end
        return require("fennel")[k]
    end,
    __newindex = function(t, k, v)
        local _ = t[k] -- call __index to ensure fennel is installed
        require("fennel")[k] = v
    end,
})

moonwalk.add_loader("fnl", function(src)
    fennel["macro-path"] = fennel["macro-path"] .. ";" .. vim.fn.stdpath("config") .. "/fnl/?.fnl"
    return fennel.compileString("(require-macros :macros)" .. src)
end)
