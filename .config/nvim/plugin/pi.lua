if vim.g.loaded_pi_nvim then
    return
end
vim.g.loaded_pi_nvim = true

vim.api.nvim_create_user_command("Pi", function(opts)
    require("pi").command(opts)
end, {
    nargs = "*",
    range = true,
    desc = "Open Pi or send it a prompt",
})

local group = vim.api.nvim_create_augroup("pi_nvim", { clear = true })
vim.api.nvim_create_autocmd("VimLeavePre", {
    group = group,
    callback = function()
        local pi = package.loaded.pi
        if pi then
            pi.cleanup()
        end
    end,
})
