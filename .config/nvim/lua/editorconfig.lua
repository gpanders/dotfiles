local function invalid(opt, val)
    vim.api.nvim_err_writeln(string.format("editorconfig: invalid value for option %s: %s", opt, val))
end

-- Modified version of glob2regpat that does not match path separators on *.
-- Basically, this replaces single instances of * with the regex pattern [^/]*.
-- However, the star in the replacement pattern also gets interpreted by
-- glob2regpat, so we insert a placeholder, pass it through glob2regpat, then
-- replace the placeholder with the actual regex pattern
local function glob2regpat(glob)
    local g = vim.fn.substitute(glob, "\\*\\@<!\\*\\*\\@!", "@@PLACEHOLDER@@", "g")
    return vim.fn.substitute(vim.fn.glob2regpat(g), "@@PLACEHOLDER@@", "[^/]*", "g")
end

local function apply(opts)
    for opt, val in pairs(opts) do
        if opt == "charset" then
            if val == "utf-8" then
                vim.bo.fileencoding = "utf-8"
                vim.bo.bomb = false
            elseif val == "utf-8-bom" then
                vim.bo.fileencoding = "utf-8"
                vim.bo.bomb = true
            elseif vim.tbl_contains({"latin1", "utf-16be", "utf-16le"}, val) then
                vim.bo.fileencoding = val
            else
                invalid(opt, val)
            end
        elseif opt == "end_of_line" then
            if val == "lf" then
                vim.bo.fileformat = "unix"
            elseif val == "crlf" then
                vim.bo.fileformat = "dos"
            elseif val == "cr" then
                vim.bo.fileformat = "mac"
            else
                invalid(opt, val)
            end
        elseif opt == "indent_style" then
            if val == "tab" then
                vim.bo.expandtab = false
                if not opts.indent_size then
                    vim.bo.shiftwidth = 0
                end
            elseif val == "space" then
                vim.bo.expandtab = true
            else
                invalid(opt, val)
            end
        elseif opt == "indent_size" then
            local n = tonumber(val)
            if n then
                vim.bo.shiftwidth = n
            else
                invalid(opt, val)
            end
        elseif opt == "tab_width" then
            local n = tonumber(val)
            if n then
                vim.bo.tabstop = n
            else
                invalid(opt, val)
            end
        elseif opt == "max_line_length" then
            local n = tonumber(val)
            if n then
                vim.bo.textwidth = n
            else
                invalid(opt, val)
            end
        elseif opt == "trim_trailing_whitespace" then
            if val == "true" and vim.fn.exists(":StripTrailingWhitespace") == 2 then
                vim.cmd('autocmd editorconfig BufWritePre <buffer> silent StripTrailingWhitespace')
            end
        elseif opt == "insert_final_newline" then
            if val == "true" then
                vim.bo.fixendofline = true
            elseif val == "false" then
                vim.bo.fixendofline = false
            else
                invalid(opt, val)
            end
        end
    end
end

local function parse(filepath, config)
    local pat
    local opts = {}
    local confdir = vim.fn.fnamemodify(config, ":h")

    for _, line in ipairs(vim.fn.readfile(config)) do
        if not line:find("^%s*$") and not line:find("^%s*[#;]") then
            local glob = string.match(line:match("%b[]") or "", "%[([^%]]+)")
            if glob then
                if glob:find("/") then
                    glob = confdir .. "/" .. glob:gsub("^/", "")
                else
                    glob = "**/" .. glob
                end
                pat = vim.regex(glob2regpat(glob))
            else
                local key, val = line:match("^%s*([^:= ][^:=]-)%s*[:=]%s*(.-)%s*$")
                if key then
                    val = val:lower()

                    if not pat and key == "root" then
                        opts.root = val == "true"
                    elseif pat:match_str(filepath) then
                        opts[key] = val
                    end
                end
            end
        end
    end

    return opts
end

return function()
    if vim.bo.buftype ~= "" or not vim.bo.modifiable then
        return
    end

    local path = vim.fn.expand("%:p")
    if path == "" then
        return
    end

    local opts = {}
    local curdir = vim.fn.fnamemodify(path, ":h")
    while true do
        local config = curdir .. "/.editorconfig"
        if vim.fn.filereadable(config) == 1 then
            opts = vim.tbl_extend("keep", opts, parse(path, config))
            if opts.root then
                break
            end
        end

        local parent = vim.fn.fnamemodify(curdir, ":h")
        if parent == curdir then
            break
        end
        curdir = parent
    end

    apply(opts)
end
