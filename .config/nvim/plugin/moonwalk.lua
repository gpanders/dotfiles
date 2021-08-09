local ok, moonwalk = pcall(require, "moonwalk")
if not ok then
    return
end

local fennel = setmetatable({}, {
    __index = function(_, k)
        local ok, fennel = pcall(require, "fennel")
        if not ok then
            local install_path = vim.fn.stdpath("data") .. "/site/pack/fennel/start/fennel"
            local tag = "0.10.0"
            print("Installing fennel " .. tag .. " to " .. install_path .. "...")
            vim.fn.system({ "git", "clone", "-b", tag, "https://git.sr.ht/~technomancy/fennel", install_path })
            vim.fn.system({ "make", "-C", install_path })
            vim.fn.system({ "mkdir", install_path .. "/lua" })
            vim.fn.system({ "mv", install_path .. "/fennel.lua", install_path .. "/lua" })
            vim.api.nvim_command("redraw")
            fennel = require("fennel")
        end
        return fennel[k]
    end,
})

moonwalk.add_loader("fnl", function(src)
    return fennel.compileString(src)
end)
