vim.filetype.add({
    extension = {
        h = function(path)
            -- Try to be a little intelligent when determining if a .h file is C++ or C

            -- If a .cc or .cpp file with the same basename exists next to this
            -- header file, assume the header is C++
            local stem = vim.fn.fnamemodify(path, ":r")
            if
                vim.uv.fs_stat(string.format("%s.cc", stem))
                or vim.uv.fs_stat(string.format("%s.cpp", stem))
            then
                return "cpp"
            end

            -- If the header file contains C++ specific keywords, assume it is
            -- C++
            if
                vim.fn.search(
                    string.format(
                        [[\C\%%(%s\)]],
                        table.concat({
                            [[^#include <[^>.]\+>$]],
                            [[\<constexpr\>]],
                            [[\<consteval\>]],
                            [[\<extern "C"\>]],
                            [[^class\> [A-Z]],
                            [[^\s*using\>]],
                            [[\<template\>\s*<]],
                            [[\<std::]],
                        }, "\\|")
                    ),
                    "nw"
                ) ~= 0
            then
                return "cpp"
            end

            return "c"
        end,
        plist = "xml",
    },
})
