if vim.g.colors_name then
    vim.api.nvim_command("hi clear")
end

vim.g.colors_name = "base16-eighties"

local gui = {
    black = "#2d2d2d",
    red = "#f2777a",
    green = "#99cc99",
    yellow = "#ffcc66",
    blue = "#6699cc",
    magenta = "#cc99cc",
    cyan = "#66cccc",
    white = "#d3d0c8",
    brblack = "#747369",
    brred = "#f99157",
    bryellow = "#515151",
    brgreen = "#393939",
    brblue = "#a09f93",
    brmagenta = "#e8e6df",
    brcyan = "#d27b53",
    brwhite = "#f2f0ec",
}

local cterm = {
    black = 0,
    red = 1,
    green = 2,
    yellow = 3,
    blue = 4,
    magenta = 5,
    cyan = 6,
    white = 7,
    brblack = 8,
    brred = 9,
    brgreen = 10,
    bryellow = 11,
    brblue = 12,
    brmagenta = 13,
    brcyan = 14,
    brwhite = 15,
}

local function hi(group, opts)
    local s = string.format(
        "hi %s ctermfg=%s ctermbg=%s guifg=%s guibg=%s cterm=%s gui=%s guisp=%s",
        group,
        cterm[opts.fg] or "NONE",
        cterm[opts.bg] or "NONE",
        gui[opts.fg] or "NONE",
        gui[opts.bg] or "NONE",
        opts.attr or "NONE",
        opts.attr or "NONE",
        gui[opts.guisp] or "NONE"
    )
    vim.api.nvim_command(s)
end

-- UI Highlights (:h highlight-groups)
hi("ColorColumn", { bg = "brgreen" })
hi("Conceal", { fg = "blue", bg = "black" })
hi("Cursor", { fg = "black", bg = "white" })
hi("CursorColumn", { bg = "brgreen", attr = "none" })
hi("CursorLine", { bg = "brgreen", attr = "none" })
hi("Directory", { fg = "blue" })
hi("DiffAdd", { fg = "green" })
hi("DiffChange", { fg = "brblack" })
hi("DiffDelete", { fg = "red" })
hi("DiffText", { fg = "blue" })
hi("ErrorMsg", { fg = "red", bg = "black" })
hi("VertSplit", { fg = "bryellow", bg = "bryellow", attr = "none" })
hi("Folded", { fg = "brblack", bg = "brgreen" })
hi("FoldColumn", { fg = "cyan", bg = "brgreen" })
hi("SignColumn", { fg = "brblack", bg = "brgreen" })
hi("IncSearch", { fg = "brgreen", bg = "brred", attr = "none" })
hi("LineNr", { fg = "brblack", bg = "brgreen" })
hi("CursorLineNr", { fg = "brblue", bg = "brgreen" })
hi("MatchParen", { bg = "brblack" })
hi("ModeMsg", { fg = "green" })
hi("MoreMsg", { fg = "green" })
hi("NonText", { fg = "brblack" })
hi("Normal", { fg = "white", bg = "black", attr = "none" })
hi("PMenu", { fg = "white", bg = "brgreen", attr = "none" })
hi("PMenuSel", { fg = "brgreen", bg = "white" })
hi("Question", { fg = "blue" })
hi("QuickFixLine", { bg = "brgreen", attr = "none" })
hi("Search", { fg = "brgreen", bg = "yellow" })
hi("SpecialKey", { fg = "brblack" })
hi("SpellBad", { fg = "red", attr = "undercurl", guisp = "red" })
hi("SpellLocal", { fg = "blue", attr = "undercurl", guisp = "cyan" })
hi("SpellCap", { fg = "magenta", attr = "undercurl", guisp = "blue" })
hi("SpellRare", { fg = "cyan", attr = "undercurl", guisp = "magenta" })
hi("StatusLine", { fg = "brblue", bg = "bryellow", attr = "none" })
hi("StatusLineNC", { fg = "brblack", bg = "brgreen", attr = "none" })
hi("TabLine", { fg = "brblack", bg = "brgreen", attr = "none" })
hi("TabLineFill", { fg = "brblack", bg = "brgreen", attr = "none" })
hi("TabLineSel", { fg = "green", bg = "brgreen", attr = "none" })
hi("Title", { fg = "blue", attr = "none" })
hi("Visual", { bg = "bryellow" })
hi("VisualNOS", { fg = "red" })
hi("WarningMsg", { fg = "yellow" })
hi("WildMenu", { fg = "brwhite", bg = "bryellow" })

-- Syntax items (:h group-name})
hi("Comment", { fg = "brblack", attr = "italic" })

hi("Constant", { fg = "brred" })
hi("String", { fg = "green" })
hi("Character", { fg = "red" })
hi("Number", { fg = "brred" })
hi("Boolean", { fg = "brred" })
hi("Float", { fg = "brred" })

hi("Identifier", { fg = "red" })
hi("Function", { fg = "blue" })

hi("Statement", { fg = "red" })
hi("Operator", { fg = "white", attr = "none" })
hi("Repeat", { fg = "red" })
hi("Conditional", { fg = "red" })
hi("Label", { fg = "yellow" })
hi("Keyword", { fg = "red" })
hi("Exception", { fg = "red" })

hi("PreProc", { fg = "yellow" })
hi("Include", { fg = "blue" })
hi("Define", { fg = "magenta" })
hi("Macro", { fg = "red" })
hi("PreCondit", { fg = "yellow" })

hi("Type", { fg = "yellow", attr = "none" })
hi("StorageClass", { fg = "yellow", attr = "none" })
hi("Structure", { fg = "magenta" })
hi("Typedef", { fg = "yellow", attr = "none" })

hi("Special", { fg = "cyan" })
hi("SpecialChar", { fg = "brcyan" })
hi("Tag", { fg = "red" })
hi("Delimiter", { fg = "brcyan" })
hi("SpecialComment", { fg = "cyan" })
hi("Debug", { fg = "red" })

hi("Underlined", { attr = "underline" })

hi("Ignore", { fg = "black", bg = "black", attr = "none" })

hi("Error", { fg = "black", bg = "red" })

hi("Todo", { fg = "yellow", bg = "brgreen" })

-- Syntax-file specific highlighting
hi("diffAdded", { fg = "green" })
hi("diffRemoved", { fg = "red" })
hi("diffLine", { fg = "cyan" })
hi("diffFile", { attr = "bold" })
hi("diffIndexLine", { attr = "bold" })

hi("gitcommitHeader", { fg = "white" })
hi("gitcommitSelectedType", { fg = "green" })
hi("gitcommitSelectedFile", { fg = "green" })
hi("gitcommitDiscardedType", { fg = "red" })
hi("gitcommitDiscardedFile", { fg = "red" })
hi("gitcommitBranch", { fg = "yellow" })

hi("gitrebasePick", { fg = "yellow" })
hi("gitrebaseReword", { fg = "blue" })
hi("gitrebaseEdit", { fg = "red" })
hi("gitrebaseSquash", { fg = "magenta" })
hi("gitrebaseFixup", { fg = "magenta" })
hi("gitrebaseExec", { fg = "cyan" })
hi("gitrebaseReset", { fg = "magenta" })

-- Plugin highlighting
hi("GitSignsAdd", { fg = "green", bg = "brgreen" })
hi("GitSignsDelete", { fg = "red", bg = "brgreen" })
hi("GitSignsChange", { fg = "brblack", bg = "brgreen" })

-- LSP Diagnostics
hi("LspDiagnosticsDefaultError", { fg = "red" })
hi("LspDiagnosticsDefaultWarning", { fg = "yellow" })
hi("LspDiagnosticsDefaultInformation", { fg = "blue" })
hi("LspDiagnosticsDefaultHint", { fg = "brblack" })
hi("LspDiagnosticsSignError", { fg = "red", bg = "brgreen" })
hi("LspDiagnosticsSignWarning", { fg = "yellow", bg = "brgreen" })
hi("LspDiagnosticsSignInformation", { fg = "blue", bg = "brgreen" })
hi("LspDiagnosticsSignHint", { fg = "brblack", bg = "brgreen" })
