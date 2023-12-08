vim.filetype.add({
    extension = {
        h = function(path)
            -- Try to be a little intelligent when determining if a .h file is C++ or C
            if
                vim.fn.search(
                    "\\C\\%(^#include <[^>.]\\+>$\\|\\<constexpr\\>\\|^class\\> [A-Z]\\|^using\\>\\|\\<std::\\)",
                    "nw"
                ) ~= 0
            then
                return "cpp"
            end

            local stem = vim.fn.fnamemodify(path, ":r")
            if
                vim.uv.fs_stat(string.format("%s.cc", stem))
                or vim.uv.fs_stat(string.format("%s.cpp", stem))
            then
                return "cpp"
            end

            return "c"
        end,
        csv = "csv",
        cl = "opencl",
        env = "env",
        plist = "xml",
    },
    filename = {
        [".envrc"] = "sh",
    },
    pattern = {
        ["~/%.config/mutt/.*"] = "muttrc",
    },
})
