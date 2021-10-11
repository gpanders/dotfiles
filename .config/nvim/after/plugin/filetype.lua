require('filetype').setup({
    overrides = {
        extensions = {
            bd = "json",
            vho = "vhdl",
            env = "env",
            cl = "opencl",
            csv = "csv",
        },
        complex = {
            ["~/.local/share/zsh/functions/*"] = "zsh",
        },
        function_extensions = {
            h = function()
                return vim.fn.search("\\C^#include <[^>.]\\+>$", "nw") ~= 0 and "cpp" or "c"
            end,
        },
    },
})
