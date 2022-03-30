local nvim = {
    buf = setmetatable({}, {
        __index = function(t, k)
            t[k] = assert(vim.api["nvim_buf_" .. k], k)
            return t[k]
        end,
    }),
    win = setmetatable({}, {
        __index = function(t, k)
            t[k] = assert(vim.api["nvim_win_" .. k], k)
            return t[k]
        end,
    }),
    current = setmetatable({}, {
        __index = function(_, k)
            return vim.api["nvim_get_current_" .. k]()
        end,
        __newindex = function(_, k, v)
            vim.api["nvim_set_current_" .. k](v)
        end,
    }),
}

_G.nvim = setmetatable(nvim, {
    __index = function(t, k)
        t[k] = assert(vim.api["nvim_" .. k], k)
        return t[k]
    end,
})
