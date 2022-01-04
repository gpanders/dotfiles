vim.g.did_load_filetypes = 0
vim.g.do_filetype_lua = 1

vim.filetype.add({
    extension = {
        h = function()
            -- Use a lazy heuristic that #including a C++ header means it's a
            -- C++ header
            if vim.fn.search("\\C^#include <[^>.]\\+>$", "nw") == 1 then
                return "cpp"
            end
            return "c"
        end,
        csv = "csv",
        cl = "opencl",
        env = "env",
    },
})
