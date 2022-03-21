local nvim = {
    buf = setmetatable({}, {
        __index = function(t, k)
            t[k] = vim.api["nvim_buf_" .. k]
            return t[k]
        end,
    }),
    win = setmetatable({}, {
        __index = function(t, k)
            t[k] = vim.api["nvim_win_" .. k]
            return t[k]
        end,
    }),
    current = setmetatable({}, {
        __index = function(t, k)
            return vim.api["nvim_get_current_" .. k]()
        end,
        __newindex = function(t, k, v)
            vim.api["nvim_set_current_" .. k](v)
        end,
    }),
}

_G.nvim = setmetatable(nvim, {
    __index = function(t, k)
        t[k] = vim.api["nvim_" .. k]
        return t[k]
    end,
})

do
    local ok, nvimrc = pcall(require, "nvimrc")
    if ok then
        nvimrc.find()
    end
end
