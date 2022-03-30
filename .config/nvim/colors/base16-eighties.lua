local nvim_set_hl = vim.api.nvim_set_hl

local colors = {
    black       = {gui = "#2d2d2d", cterm = 0},
    red         = {gui = "#f2777a", cterm = 1},
    green       = {gui = "#99cc99", cterm = 2},
    yellow      = {gui = "#ffcc66", cterm = 3},
    blue        = {gui = "#6699cc", cterm = 4},
    magenta     = {gui = "#cc99cc", cterm = 5},
    cyan        = {gui = "#66cccc", cterm = 6},
    normal      = {gui = "#d3d0c8", cterm = 7},
    lightgray   = {gui = "#747369", cterm = 8},
    orange      = {gui = "#f99157", cterm = 9},
    darkgray    = {gui = "#393939", cterm = 10},
    gray        = {gui = "#515151", cterm = 11},
    lightergray = {gui = "#a09f93", cterm = 12},
    white       = {gui = "#e8e6df", cterm = 13},
    darkorange  = {gui = "#d27b53", cterm = 14},
    brwhite     = {gui = "#f2f0ec", cterm = 15},
}

local function hi(name)
    return function(opts)
        nvim_set_hl(0, name, {
            link = opts.link,
            ctermfg = opts.fg and colors[opts.fg].cterm,
            ctermbg = opts.bg and colors[opts.bg].cterm,
            fg = opts.fg and colors[opts.fg].gui,
            bg = opts.bg and colors[opts.bg].gui,
            sp = opts.sp and colors[opts.sp].gui,
            bold = opts.attr == "bold" and 1 or 0,
            italic = opts.attr == "italic" and 1 or 0,
            underline = opts.attr == "underline" and 1 or 0,
            undercurl = opts.attr == "undercurl" and 1 or 0,
        })
    end
end

hi "Normal" {fg = "normal", bg = "black"}
hi "None" {fg = "normal"}

-- UI Highlights (:h highlight-groups)
hi "ColorColumn" {bg = "darkgray"}
hi "Conceal" {fg = "blue"}
hi "Cursor" {fg = "black", bg = "normal"}
hi "CursorColumn" {bg = "darkgray"}
hi "CursorLine" {bg = "darkgray"}
hi "CursorLineNr" {fg = "lightergray", bg = "darkgray"}
hi "DiffAdd" {fg = "green"}
hi "DiffChange" {fg = "lightgray"}
hi "DiffDelete" {fg = "red"}
hi "DiffText" {fg = "blue"}
hi "Directory" {fg = "blue"}
hi "ErrorMsg" {fg = "red"}
hi "FloatBorder" {bg = "black"}
hi "FoldColumn" {fg = "cyan", bg = "darkgray"}
hi "Folded" {fg = "lightgray", bg = "darkgray"}
hi "IncSearch" {fg = "darkgray", bg = "orange"}
hi "LineNr" {fg = "lightgray", bg = "darkgray"}
hi "MatchParen" {bg = "lightgray"}
hi "MatchWord" {bg = "darkgray"}
hi "ModeMsg" {fg = "green"}
hi "MoreMsg" {fg = "green"}
hi "NonText" {fg = "lightgray"}
hi "NormalFloat" {bg = "darkgray"}
hi "PMenu" {bg = "darkgray"}
hi "PMenuSel" {fg = "darkgray", bg = "normal"}
hi "Question" {fg = "blue"}
hi "QuickFixLine" {bg = "darkgray"}
hi "Search" {fg = "darkgray", bg = "yellow"}
hi "SignColumn" {fg = "lightgray", bg = "darkgray"}
hi "SpecialKey" {fg = "green"}
hi "SpellBad" {attr = "undercurl", sp = "red"}
hi "SpellCap" {attr = "undercurl", sp = "magenta"}
hi "SpellLocal" {attr = "undercurl", sp = "blue"}
hi "SpellRare" {attr = "undercurl", sp = "cyan"}
hi "StatusLine" {fg = "lightergray", bg = "gray"}
hi "StatusLineNC" {fg = "lightgray", bg = "darkgray"}
hi "TabLine" {fg = "lightgray", bg = "darkgray"}
hi "TabLineFill" {fg = "lightgray", bg = "darkgray"}
hi "TabLineSel" {fg = "green", bg = "darkgray"}
hi "Title" {fg = "blue"}
hi "VertSplit" {fg = "gray", bg = "gray"}
hi "Visual" {bg = "gray"}
hi "VisualNOS" {fg = "red"}
hi "WarningMsg" {fg = "yellow"}
hi "WildMenu" {fg = "brwhite", bg = "gray"}

-- Syntax items (:h group-name)
hi "Comment" {fg = "lightgray"}

hi "Constant" {}
hi "String" {fg = "green"}
hi "Character" {fg = "red"}
hi "Number" {link = "Constant"}
hi "Boolean" {link = "Constant"}
hi "Float" {link = "Constant"}

hi "Identifier" {}
hi "Function" {fg = "blue"}

hi "Statement" {fg = "red"}
hi "Operator" {}
hi "Repeat" {link = "Statement"}
hi "Conditional" {link = "Statement"}
hi "Label" {}
hi "Keyword" {link = "Statement"}
hi "Exception" {link = "Statement"}

hi "PreProc" {fg = "magenta"}
hi "Include" {fg = "blue"}
hi "Define" {fg = "magenta"}
hi "Macro" {fg = "magenta"}
hi "PreCondit" {link = "PreProc"}

hi "Type" {fg = "yellow"}
hi "StorageClass" {link = "Type"}
hi "Structure" {fg = "yellow"}
hi "Typedef" {link = "Type"}

hi "Special" {fg = "cyan"}
hi "SpecialChar" {link = "Special"}
hi "Tag" {fg = "red"}
hi "Delimiter" {}
hi "SpecialComment" {link = "Special"}
hi "Debug" {fg = "red"}

hi "Underlined" {fg = "normal", attr = "underline"}
hi "Bold" {attr = "bold"}

hi "Ignore" {fg = "black"}

hi "Error" {fg = "red"}

hi "Todo" {fg = "yellow", bg = "darkgray"}

hi "DiagnosticError" {fg = "red"}
hi "DiagnosticWarn" {fg = "yellow"}
hi "DiagnosticInfo" {fg = "blue"}
hi "DiagnosticHint" {fg = "lightgray"}
hi "DiagnosticSignError" {fg = "red", bg = "darkgray"}
hi "DiagnosticSignWarn" {fg = "yellow", bg = "darkgray"}
hi "DiagnosticSignInfo" {fg = "blue", bg = "darkgray"}
hi "DiagnosticSignHint" {fg = "lightgray", bg = "darkgray"}
hi "DiagnosticUnderlineError" {attr = "undercurl", sp = "red"}
hi "DiagnosticUnderlineWarn" {attr = "undercurl", sp = "yellow"}
hi "DiagnosticUnderlineInfo" {attr = "undercurl", sp = "blue"}
hi "DiagnosticUnderlineHint" {attr = "undercurl", sp = "lightgray"}

-- Syntax-file specific highlighting
hi "diffAdded" {fg = "green"}
hi "diffRemoved" {fg = "red"}
hi "diffLine" {fg = "cyan"}
hi "diffFile" {attr = "bold"}
hi "diffIndexLine" {attr = "bold"}
hi "diffSubname" {fg = "normal"}

hi "fennelSpecialForm" {fg = "red"}
hi "fennelKeyword" {}

hi "gitHash" {fg = "yellow"}
hi "gitKeyword" {fg = "yellow"}
hi "gitcommitHeader" {fg = "normal"}
hi "gitcommitSummary" {attr = "bold"}
hi "gitcommitSelectedType" {fg = "green"}
hi "gitcommitSelectedFile" {fg = "green"}
hi "gitcommitDiscardedType" {fg = "red"}
hi "gitcommitDiscardedFile" {fg = "red"}
hi "gitcommitUntrackedFile" {fg = "red"}
hi "gitcommitBranch" {fg = "yellow"}
hi "gitrebaseHash" {fg = "yellow"}
hi "gitrebaseSummary" {fg = "normal"}
hi "gitrebasePick" {fg = "green"}
hi "gitrebaseReword" {fg = "blue"}
hi "gitrebaseEdit" {fg = "red"}
hi "gitrebaseSquash" {fg = "cyan"}
hi "gitrebaseFixup" {fg = "cyan"}
hi "gitrebaseExec" {fg = "cyan"}
hi "gitrebaseReset" {fg = "cyan"}

hi "manSubHeading" {fg = "red", attr = "bold"}
hi "manOptionDesc" {fg = "red", attr = "bold"}
hi "manReference" {fg = "yellow"}
hi "manUnderline" {fg = "green", attr = "bold"}

hi "markdownRule" {fg = "yellow"}
hi "markdownHeadingDelimiter" {fg = "darkorange"}

hi "helpHeader" {fg = "yellow"}
hi "helpSectionDelim" {fg = "lightgray"}
hi "helpOption" {fg = "yellow"}
hi "helpHyperTextJump" {fg = "red"}

hi "luaTable" {}
hi "luaSpecialValue" {fg = "blue"}

hi "shShellVariables" {fg = "yellow"}

hi "vimOption" {fg = "normal"}
hi "vimEnvvar" {}
hi "vimVar" {}
hi "vimFuncvar" {}
hi "vimSpecial" {}

hi "zigLabel" {fg = "cyan"}

-- Plugin highlighting
hi "GitSignsAdd" {fg = "green", bg = "darkgray"}
hi "GitSignsDelete" {fg = "red", bg = "darkgray"}
hi "GitSignsChange" {fg = "lightgray", bg = "darkgray"}

hi "packerHash" {fg = "yellow"}

hi "TSNone" {fg = "normal"}
hi "TSSymbol" {fg = "yellow"}
hi "TSDefinition" {link = "MatchWord"}
hi "TSDefinitionUsage" {link = "MatchWord"}
hi "TSParameter" {}
hi "TSPunctBrakcet" {}
hi "TSPunctDelimiter" {}
hi "TSField" {}
hi "TSProperty" {}
hi "TSNamespace" {}
hi "TSConstructor" {}
hi "TSFuncBuiltin" {link = "Function"}
hi "TSFuncMacro" {link = "Function"}
hi "TSConstBuiltin" {link = "Constant"}

hi "LspReferenceText" {link = "MatchWord"}
hi "LspReferenceRead" {link = "MatchWord"}
hi "LspReferenceWrite" {link = "MatchWord"}

hi "debugPC" {fg = "cyan", bg = "darkgray"}
hi "debugBreakpoint" {fg = "red", bg = "darkgray"}

hi "Sneak" {link = "IncSearch"}

hi "TelescopeMatching" {fg = "yellow"}

hi "TreesitterContext" {bg = "darkgray"}
