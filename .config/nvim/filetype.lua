vim.filetype.add({
    extension = {
        h = function()
            -- Try to be a little intelligent when determining if a .h file is C++ or C
            if
                vim.fn.search(
                    "\\C\\%(^#include <[^>.]\\+>$\\|\\<constexpr\\>\\|^class\\> [A-Z]\\|^using\\>\\|\\<std::\\)",
                    "nw"
                ) ~= 0
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
    pattern = {
        ["~/%.config/mutt/.*"] = "muttrc",
    },
})
