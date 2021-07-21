function! s:load()
    inoremap <expr> <Tab> v:lua.check_snippet() ? '<Cmd>lua require("snippets").expand_or_advance(1)<CR>' : '<Tab>'
    inoremap <S-Tab> <Cmd>lua require("snippets").advance_snippet(-1)<CR>
lua <<
local ok, snippets = pcall(require, "snippets")
if not ok then
    return
end

function _G.check_snippet()
    if snippets.has_active_snippet() then
        return true
    end

    local _, snippet = snippets.lookup_snippet_at_cursor()
    return snippet ~= nil
end

function _G.split_getopts(str)
    local lines = {}
    for c in str:gmatch("(%a[:]?)") do
        if c:find(":") then
            table.insert(lines, string.format("\t\t%s) echo \"$OPTARG\" ;;", c:sub(1, 1)))
        else
            table.insert(lines, string.format("\t\t%s) ;;", c))
        end
    end

    return table.concat(lines, "\n")
end

local U = require("snippets.utils")

snippets.ux = require("snippets.inserters.extmarks")

-- Map from snippet file extension to filetype
local ext_ft_map = {
    ["py"] = "python",
    ["rs"] = "rust",
}

-- Read snippets from snippets directory
local snippets_dir = vim.fn.stdpath("config") .. "/snippets"
local function read_snippets(t)
    local s = t or {}
    for filename in vim.gsplit(vim.fn.glob(snippets_dir .. "/*"), "\n") do
        local f = io.open(filename)
        if f then
            local name, ext = filename:match("^.+/([^/]+)%.([^.]+)$")
            local ft = ext_ft_map[ext] or ext
            if not s[ft] then
                s[ft] = {}
            end

            local snippet = f:read("*all")
            f:close()

            -- Strip trailing newline
            snippet = snippet:gsub("\n$", "")

            if snippet:find("\n") then
                snippet = U.match_indentation(snippet)
            end

            s[ft][name] = snippet
        end
    end

    return s
end

snippets.snippets = read_snippets({
    _global = {
        copyright = U.force_comment([[Copyright (C) ${=os.date("%Y")} Gregory Anders]]),
        GPL = (function()
            local S = U.force_comment([[
Copyright (C) ${=os.date("%Y")} Gregory Anders

SPDX-License-Identifier: GPL-3.0-or-later

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.]])
            table.insert(S, "\n")
            table.insert(S, "\n")
            return S
        end)(),
    },
})
.
endfunction

augroup my_snippets
    autocmd! InsertEnter * ++once call s:load()
augroup END
