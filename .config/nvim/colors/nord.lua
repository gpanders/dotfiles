local nvim_set_hl = vim.api.nvim_set_hl

vim.o.background = "dark"

local colors = {
    background    = { gui = "#2E3440", cterm = 0  },
    darkblack     = { gui = "#373E4D", cterm = 0  },
    black         = { gui = "#3B4252", cterm = 0  },
    lightblack    = { gui = "#434C5E", cterm = 2  },
    brightblack   = { gui = "#4C566A", cterm = 8  },
    brighterblack = { gui = "#616E88", cterm = 4  },
    foreground    = { gui = "#D8DEE9", cterm = 7  },
    darkwhite     = { gui = "#AEB3BB", cterm = 7  },
    white         = { gui = "#E5E9F0", cterm = 7  },
    brightwhite   = { gui = "#ECEFF4", cterm = 15 },
    brightcyan    = { gui = "#8FBCBB", cterm = 14 },
    cyan          = { gui = "#88C0D0", cterm = 6  },
    blue          = { gui = "#81A1C1", cterm = 4  },
    darkblue      = { gui = "#5E81AC", cterm = 12 },
    red           = { gui = "#BF616A", cterm = 1  },
    orange        = { gui = "#D08770", cterm = 11 },
    yellow        = { gui = "#EBCB8B", cterm = 3  },
    green         = { gui = "#A3BE8C", cterm = 2  },
    magenta       = { gui = "#B48EAD", cterm = 5  },
}

vim.g.terminal_color_0 = colors.black.gui
vim.g.terminal_color_1 = colors.red.gui
vim.g.terminal_color_2 = colors.green.gui
vim.g.terminal_color_3 = colors.yellow.gui
vim.g.terminal_color_4 = colors.blue.gui
vim.g.terminal_color_5 = colors.magenta.gui
vim.g.terminal_color_6 = colors.cyan.gui
vim.g.terminal_color_7 = colors.white.gui
vim.g.terminal_color_8 = colors.brightblack.gui
vim.g.terminal_color_9 = colors.red.gui
vim.g.terminal_color_10 = colors.green.gui
vim.g.terminal_color_11 = colors.yellow.gui
vim.g.terminal_color_12 = colors.blue.gui
vim.g.terminal_color_13 = colors.magenta.gui
vim.g.terminal_color_14 = colors.brightcyan.gui
vim.g.terminal_color_15 = colors.brightwhite.gui

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

hi "Bold" { attr = "bold" }
hi "Italic" {}
hi "Underline" { attr = "underline" }

-- Editor
hi "ColorColumn" { bg = "black" }
hi "Cursor" { fg = "background", bg = "foreground" }
hi "CursorLine" { bg = "black" }
hi "Error" { fg = "foreground", bg = "red" }
hi "iCursor" { fg = "background", bg = "foreground" }
hi "LineNr" { fg = "brighterblack", bg = "darkblack" }
hi "MatchParen" { fg = "cyan", bg = "brightblack" }
hi "NonText" { fg = "lightblack" }
hi "Normal" { fg = "foreground", bg = "background" }
hi "Pmenu" { fg = "foreground", bg = "lightblack" }
hi "PmenuSbar" { fg = "foreground", bg = "lightblack" }
hi "PmenuSel" { fg = "cyan", bg = "brightblack" }
hi "PmenuThumb" { fg = "cyan", bg = "brightblack" }
hi "SpecialKey" { fg = "brightblack" }
hi "SpellBad" { attr = "undercurl", sp = "red" }
hi "SpellCap" { attr = "undercurl", sp = "yellow" }
hi "SpellLocal" { attr = "undercurl", sp = "white" }
hi "SpellRare" { attr = "undercurl", sp = "brightwhite" }
hi "Visual" { bg = "lightblack" }
hi "VisualNOS" { bg = "lightblack" }

-- Neovim Support
hi "healthError" { fg = "red", bg = "black" }
hi "healthSuccess" { fg = "green", bg = "black" }
hi "healthWarning" { fg = "yellow", bg = "black" }
hi "TermCursorNC" { bg = "black" }

hi "DiagnosticWarn" { fg = "yellow" }
hi "DiagnosticError" { fg = "red" }
hi "DiagnosticInfo" { fg = "cyan" }
hi "DiagnosticHint" { fg = "darkblue" }
hi "DiagnosticSignWarn" { fg = "yellow", bg = "black" }
hi "DiagnosticSignError" { fg = "red", bg = "black" }
hi "DiagnosticSignInfo" { fg = "cyan", bg = "black" }
hi "DiagnosticSignHint" { fg = "darkblue", bg = "black" }
hi "DiagnosticUnderlineWarn" { sp = "yellow", attr = "undercurl" }
hi "DiagnosticUnderlineError" { sp = "red", attr = "undercurl" }
hi "DiagnosticUnderlineInfo" { sp = "cyan", attr = "undercurl" }
hi "DiagnosticUnderlineHint" { sp = "darkblue", attr = "undercurl" }

-- Neovim DocumentHighlight
hi "LspReferenceText" { bg = "brightblack" }
hi "LspReferenceRead" { bg = "brightblack" }
hi "LspReferenceWrite" { bg = "brightblack" }

-- Neovim LspSignatureHelp
hi "LspSignatureActiveParameter" { fg = "cyan", attr = "underline" }

-- Gutter
hi "CursorColumn" { bg = "darkblack" }
hi "CursorLineNr" { fg = "foreground", bg = "darkblack" }
hi "Folded" { fg = "brightblack", bg = "darkblack", attr = "bold" }
hi "FoldColumn" { fg = "brightblack", bg = "darkblack" }
hi "SignColumn" { fg = "black", bg = "darkblack" }

-- Navigation
hi "Directory" { fg = "blue" }

-- Prompt/Status
hi "EndOfBuffer" { fg = "black" }
hi "ErrorMsg" { fg = "foreground", bg = "red" }
hi "ModeMsg" { fg = "foreground" }
hi "MoreMsg" { fg = "cyan" }
hi "Question" { fg = "foreground" }
hi "StatusLine" { fg = "foreground", bg = "lightblack" }
hi "StatusLineNC" { fg = "brighterblack", bg = "darkblack" }
hi "StatusLineTerm" { fg = "foreground", bg = "lightblack" }
hi "StatusLineTermNC" { fg = "brighterblack", bg = "darkblack" }
hi "WarningMsg" { fg = "background", bg = "yellow" }
hi "WildMenu" { fg = "cyan", bg = "black" }

-- Search
hi "IncSearch" { fg = "brightwhite", bg = "darkblue", attr = "underline" }
hi "Search" { fg = "black", bg = "cyan" }

-- Tabs
hi "TabLine" { fg = "foreground", bg = "black" }
hi "TabLineFill" { fg = "foreground", bg = "black" }
hi "TabLineSel" { fg = "cyan", bg = "brightblack" }

-- Window
hi "Title" { fg = "foreground" }

hi "VertSplit" { fg = "darkblack", bg = "darkblack" }

hi "QuickFixLine" { link = "Visual" }

hi "User1" { fg = "foreground", bg = "lightblack", attr = "bold" }
hi "User2" { fg = "red", bg = "lightblack", attr = "bold" }

-----------------------
-- Language Base Groups
-----------------------
hi "Boolean" { fg = "blue" }
hi "Character" { fg = "green" }
hi "Comment" { fg = "brighterblack" }
hi "Conceal" {}
hi "Conditional" { fg = "blue" }
hi "Constant" { fg = "foreground" }
hi "Decorator" { fg = "orange" }
hi "Define" { fg = "blue" }
hi "Delimiter" { fg = "brightwhite" }
hi "Exception" { fg = "blue" }
hi "Float" { fg = "magenta" }
hi "Function" { fg = "cyan" }
hi "Identifier" { fg = "foreground" }
hi "Include" { fg = "blue" }
hi "Keyword" { fg = "blue" }
hi "Label" { fg = "blue" }
hi "Number" { fg = "magenta" }
hi "Operator" { fg = "blue" }
hi "PreProc" { fg = "blue" }
hi "Repeat" { fg = "blue" }
hi "Special" { fg = "cyan" }
hi "SpecialChar" { fg = "yellow" }
hi "SpecialComment" { fg = "cyan" }
hi "Statement" { fg = "blue" }
hi "StorageClass" { fg = "blue" }
hi "String" { fg = "green" }
hi "Structure" { fg = "blue" }
hi "Tag" { fg = "foreground" }
hi "Todo" { fg = "yellow" }
hi "Type" { fg = "blue" }
hi "Typedef" { fg = "blue" }
hi "Annotation" { link = "Decorator" }
hi "Macro" { link = "Define" }
hi "PreCondit" { link = "PreProc" }
hi "Variable" { link = "Identifier" }

-------------
-- Languages
-------------
hi "asciidocAttributeEntry" { fg = "darkblue" }
hi "asciidocAttributeList" { fg = "darkblue" }
hi "asciidocAttributeRef" { fg = "darkblue" }
hi "asciidocHLabel" { fg = "blue" }
hi "asciidocListingBlock" { fg = "brightcyan" }
hi "asciidocMacroAttributes" { fg = "cyan" }
hi "asciidocOneLineTitle" { fg = "cyan" }
hi "asciidocPassthroughBlock" { fg = "blue" }
hi "asciidocQuotedMonospaced" { fg = "brightcyan" }
hi "asciidocTriplePlusPassthrough" { fg = "brightcyan" }
hi "asciidocAdmonition" { link = "Keyword" }
hi "asciidocAttributeRef" { link = "markdownH1" }
hi "asciidocBackslash" { link = "Keyword" }
hi "asciidocMacro" { link = "Keyword" }
hi "asciidocQuotedBold" { link = "Bold" }
hi "asciidocQuotedEmphasized" { link = "Italic" }
hi "asciidocQuotedMonospaced2" { link = "asciidocQuotedMonospaced" }
hi "asciidocQuotedUnconstrainedBold" { link = "asciidocQuotedBold" }
hi "asciidocQuotedUnconstrainedEmphasized" { link = "asciidocQuotedEmphasized" }
hi "asciidocURL" { link = "markdownLinkText" }

hi "awkCharClass" { fg = "brightcyan" }
hi "awkPatterns" { fg = "blue", attr = "bold" }
hi "awkArrayElement" { link = "Identifier" }
hi "awkBoolLogic" { link = "Keyword" }
hi "awkBrktRegExp" { link = "SpecialChar" }
hi "awkComma" { link = "Delimiter" }
hi "awkExpression" { link = "Keyword" }
hi "awkFieldVars" { link = "Identifier" }
hi "awkLineSkip" { link = "Keyword" }
hi "awkOperator" { link = "Operator" }
hi "awkRegExp" { link = "SpecialChar" }
hi "awkSearch" { link = "Keyword" }
hi "awkSemicolon" { link = "Delimiter" }
hi "awkSpecialCharacter" { link = "SpecialChar" }
hi "awkSpecialPrintf" { link = "SpecialChar" }
hi "awkVariables" { link = "Identifier" }

hi "cIncluded" { fg = "brightcyan" }
hi "cOperator" { link = "Operator" }
hi "cPreCondit" { link = "PreCondit" }
hi "cConstant" { link = "Type" }

hi "cmakeGeneratorExpression" { fg = "darkblue" }

hi "csPreCondit" { link = "PreCondit" }
hi "csType" { link = "Type" }
hi "csXmlTag" { link = "SpecialComment" }

hi "cssAttributeSelector" { fg = "brightcyan" }
hi "cssDefinition" { fg = "brightcyan" }
hi "cssIdentifier" { fg = "brightcyan", attr = "underline" }
hi "cssStringQ" { fg = "brightcyan" }
hi "cssAttr" { link = "Keyword" }
hi "cssBraces" { link = "Delimiter" }
hi "cssClassName" { link = "cssDefinition" }
hi "cssColor" { link = "Number" }
hi "cssProp" { link = "cssDefinition" }
hi "cssPseudoClass" { link = "cssDefinition" }
hi "cssPseudoClassId" { link = "cssPseudoClass" }
hi "cssVendor" { link = "Keyword" }

hi "dosiniHeader" { fg = "cyan" }
hi "dosiniLabel" { link = "Type" }

hi "dtBooleanKey" { fg = "brightcyan" }
hi "dtExecKey" { fg = "brightcyan" }
hi "dtLocaleKey" { fg = "brightcyan" }
hi "dtNumericKey" { fg = "brightcyan" }
hi "dtTypeKey" { fg = "brightcyan" }
hi "dtDelim" { link = "Delimiter" }
hi "dtLocaleValue" { link = "Keyword" }
hi "dtTypeValue" { link = "Keyword" }

hi "DiffAdd" { fg = "green", bg = "background", attr = "inverse" }
hi "DiffChange" { fg = "yellow", bg = "background", attr = "inverse" }
hi "DiffDelete" { fg = "red", bg = "background", attr = "inverse" }
hi "DiffText" { fg = "blue", bg = "background", attr = "inverse" }

-- Legacy groups for official git.vim and diff.vim syntax
hi "diffAdded" { link = "DiffAdd" }
hi "diffChanged" { link = "DiffChange" }
hi "diffRemoved" { link = "DiffDelete" }

hi "gitconfigVariable" { fg = "brightcyan" }
hi "gitrebaseFixup" { fg = "cyan" }
hi "gitrebaseExec" { fg = "cyan" }
hi "gitrebaseReword" { fg = "yellow" }

hi "goBuiltins" { fg = "brightcyan" }
hi "goConstants" { link = "Keyword" }

hi "haskellPreProc" { fg = "darkblue" }
hi "haskellType" { fg = "brightcyan" }
hi "haskellPragma" { link = "haskellPreProc" }

hi "helpBar" { fg = "brightblack" }
hi "helpHyperTextJump" { fg = "cyan", attr = "underline" }

hi "htmlArg" { fg = "brightcyan" }
hi "htmlLink" { fg = "foreground" }
hi "htmlBold" { link = "Bold" }
hi "htmlEndTag" { link = "htmlTag" }
hi "htmlItalic" { link = "Italic" }
hi "htmlH1" { link = "markdownH1" }
hi "htmlH2" { link = "markdownH1" }
hi "htmlH3" { link = "markdownH1" }
hi "htmlH4" { link = "markdownH1" }
hi "htmlH5" { link = "markdownH1" }
hi "htmlH6" { link = "markdownH1" }
hi "htmlSpecialChar" { link = "SpecialChar" }
hi "htmlTag" { link = "Keyword" }
hi "htmlTagN" { link = "htmlTag" }

hi "javaDocTags" { fg = "brightcyan" }
hi "javaCommentTitle" { link = "Comment" }
hi "javaScriptBraces" { link = "Delimiter" }
hi "javaScriptIdentifier" { link = "Keyword" }
hi "javaScriptNumber" { link = "Number" }

hi "jsGlobalNodeObjects" { fg = "cyan" }
hi "jsBrackets" { link = "Delimiter" }
hi "jsFuncCall" { link = "Function" }
hi "jsFuncParens" { link = "Delimiter" }
hi "jsThis" { link = "Keyword" }
hi "jsNoise" { link = "Delimiter" }
hi "jsPrototype" { link = "Keyword" }
hi "jsRegexpString" { link = "SpecialChar" }

hi "jsonKeyword" { fg = "brightcyan" }

hi "lessClass" { fg = "brightcyan" }
hi "lessAmpersand" { link = "Keyword" }
hi "lessCssAttribute" { link = "Delimiter" }
hi "lessFunction" { link = "Function" }
hi "cssSelectorOp" { link = "Keyword" }

hi "lispAtomBarSymbol" { link = "SpecialChar" }
hi "lispAtomList" { link = "SpecialChar" }
hi "lispAtomMark" { link = "Keyword" }
hi "lispBarSymbol" { link = "SpecialChar" }
hi "lispFunc" { link = "Function" }

hi "luaFunc" { link = "Function" }

hi "markdownBlockquote" { fg = "brightcyan" }
hi "markdownCode" { fg = "brightcyan" }
hi "markdownCodeDelimiter" { fg = "brightcyan" }
hi "markdownFootnote" { fg = "brightcyan" }
hi "markdownId" { fg = "brightcyan" }
hi "markdownIdDeclaration" { fg = "brightcyan" }
hi "markdownH1" { fg = "cyan" }
hi "markdownLinkText" { fg = "cyan" }
hi "markdownUrl" { fg = "foreground" }
hi "markdownBold" { link = "Bold" }
hi "markdownBoldDelimiter" { link = "Keyword" }
hi "markdownFootnoteDefinition" { link = "markdownFootnote" }
hi "markdownH2" { link = "markdownH1" }
hi "markdownH3" { link = "markdownH1" }
hi "markdownH4" { link = "markdownH1" }
hi "markdownH5" { link = "markdownH1" }
hi "markdownH6" { link = "markdownH1" }
hi "markdownIdDelimiter" { link = "Keyword" }
hi "markdownItalic" { link = "Italic" }
hi "markdownItalicDelimiter" { link = "Keyword" }
hi "markdownLinkDelimiter" { link = "Keyword" }
hi "markdownLinkTextDelimiter" { link = "Keyword" }
hi "markdownListMarker" { link = "Keyword" }
hi "markdownRule" { link = "Keyword" }
hi "markdownHeadingDelimiter" { link = "Keyword" }

hi "pandocDefinitionBlockTerm" { fg = "brightcyan" }
hi "pandocTableDelims" { fg = "brightblack" }
hi "pandocAtxHeader" { link = "markdownH1" }
hi "pandocBlockQuote" { link = "markdownBlockquote" }
hi "pandocCiteAnchor" { link = "Operator" }
hi "pandocCiteKey" { link = "pandocReferenceLabel" }
hi "pandocDefinitionBlockMark" { link = "Operator" }
hi "pandocEmphasis" { link = "markdownItalic" }
hi "pandocFootnoteID" { link = "pandocReferenceLabel" }
hi "pandocFootnoteIDHead" { link = "markdownLinkDelimiter" }
hi "pandocFootnoteIDTail" { link = "pandocFootnoteIDHead" }
hi "pandocGridTableDelims" { link = "pandocTableDelims" }
hi "pandocGridTableHeader" { link = "pandocTableDelims" }
hi "pandocOperator" { link = "Operator" }
hi "pandocPipeTableDelims" { link = "pandocTableDelims" }
hi "pandocReferenceDefinition" { link = "pandocReferenceLabel" }
hi "pandocReferenceLabel" { link = "markdownLinkText" }
hi "pandocReferenceURL" { link = "markdownUrl" }
hi "pandocSimpleTableHeader" { link = "pandocAtxHeader" }
hi "pandocStrong" { link = "markdownBold" }
hi "pandocTableHeaderWord" { link = "pandocAtxHeader" }
hi "pandocUListItemBullet" { link = "Operator" }

hi "perlPackageDecl" { fg = "brightcyan" }

hi "phpClasses" { fg = "brightcyan" }
hi "phpDocTags" { fg = "brightcyan" }
hi "phpDocCustomTags" { link = "phpDocTags" }
hi "phpMemberSelector" { link = "Keyword" }
hi "phpClass" { fg = "brightcyan" }
hi "phpClassImplements" { fg = "brightcyan", attr = "bold" }
hi "phpClassExtends" { link = "phpClass" }
hi "phpFunction" { link = "Function" }
hi "phpMethod" { link = "Function" }
hi "phpUseClass" { link = "phpClass" }

hi "podCmdText" { fg = "brightcyan" }
hi "podVerbatimLine" { fg = "foreground" }
hi "podFormat" { link = "Keyword" }

hi "pythonBuiltin" { link = "Type" }
hi "pythonEscape" { link = "SpecialChar" }

hi "rubyConstant" { fg = "brightcyan" }
hi "rubySymbol" { fg = "brightwhite", attr = "bold" }
hi "rubyAttribute" { link = "Identifier" }
hi "rubyBlockParameterList" { link = "Operator" }
hi "rubyInterpolationDelimiter" { link = "Keyword" }
hi "rubyKeywordAsMethod" { link = "Function" }
hi "rubyLocalVariableOrMethod" { link = "Function" }
hi "rubyPseudoVariable" { link = "Keyword" }
hi "rubyRegexp" { link = "SpecialChar" }

hi "rustAttribute" { fg = "darkblue" }
hi "rustEnum" { fg = "brightcyan", attr = "bold" }
hi "rustMacro" { fg = "cyan", attr = "bold" }
hi "rustModPath" { fg = "brightcyan" }
hi "rustPanic" { fg = "blue", attr = "bold" }
hi "rustTrait" { fg = "brightcyan" }
hi "rustCommentLineDoc" { link = "Comment" }
hi "rustDerive" { link = "rustAttribute" }
hi "rustEnumVariant" { link = "rustEnum" }
hi "rustEscape" { link = "SpecialChar" }
hi "rustQuestionMark" { link = "Keyword" }

hi "sassClass" { fg = "brightcyan" }
hi "sassId" { fg = "brightcyan", attr = "underline" }
hi "sassAmpersand" { link = "Keyword" }
hi "sassClassChar" { link = "Delimiter" }
hi "sassControl" { link = "Keyword" }
hi "sassControlLine" { link = "Keyword" }
hi "sassExtend" { link = "Keyword" }
hi "sassFor" { link = "Keyword" }
hi "sassFunctionDecl" { link = "Keyword" }
hi "sassFunctionName" { link = "Function" }
hi "sassidChar" { link = "sassId" }
hi "sassInclude" { link = "SpecialChar" }
hi "sassMixinName" { link = "Function" }
hi "sassMixing" { link = "SpecialChar" }
hi "sassReturn" { link = "Keyword" }

hi "shCmdParenRegion" { link = "Delimiter" }
hi "shCmdSubRegion" { link = "Delimiter" }
hi "shDerefSimple" { link = "Identifier" }
hi "shDerefVar" { link = "Identifier" }

hi "sqlKeyword" { link = "Keyword" }
hi "sqlSpecial" { link = "Keyword" }

hi "tsxAttrib" { fg = "brightcyan" }
hi "tsxEqual" { link = "Operator" }
hi "tsxIntrinsicTagName" { link = "htmlTag" }
hi "tsxTagName" { link = "tsxIntrinsicTagName" }

hi "typescriptBOMWindowMethod" { fg = "cyan" }
hi "typescriptClassName" { fg = "brightcyan" }
hi "typescriptDecorator" { fg = "orange" }
hi "typescriptInterfaceName" { fg = "brightcyan", attr = "bold" }
hi "typescriptRegexpString" { fg = "yellow" }
hi "typescriptOperator" { link = "Operator" }
hi "typescriptBinaryOp" { link = "Operator" }
hi "typescriptAssign" { link = "Operator" }
hi "typescriptMember" { link = "Identifier" }
hi "typescriptDOMStorageMethod" { link = "Identifier" }
hi "typescriptArrowFuncArg" { link = "Identifier" }
hi "typescriptGlobal" { link = "typescriptClassName" }
hi "typescriptBOMWindowProp" { link = "Function" }
hi "typescriptArrowFuncDef" { link = "Function" }
hi "typescriptAliasDeclaration" { link = "Function" }
hi "typescriptPredefinedType" { link = "Type" }
hi "typescriptTypeReference" { link = "typescriptClassName" }
hi "typescriptTypeAnnotation" { link = "Structure" }
hi "typescriptDocNamedParamType" { link = "SpecialComment" }
hi "typescriptDocNotation" { link = "Keyword" }
hi "typescriptDocTags" { link = "Keyword" }
hi "typescriptImport" { link = "Keyword" }
hi "typescriptExport" { link = "Keyword" }
hi "typescriptTry" { link = "Keyword" }
hi "typescriptVariable" { link = "Keyword" }
hi "typescriptBraces" { link = "Normal" }
hi "typescriptObjectLabel" { link = "Normal" }
hi "typescriptCall" { link = "Normal" }
hi "typescriptClassHeritage" { link = "typescriptClassName" }
hi "typescriptFuncTypeArrow" { link = "Structure" }
hi "typescriptMemberOptionality" { link = "Structure" }
hi "typescriptNodeGlobal" { link = "typescriptGlobal" }
hi "typescriptTypeBrackets" { link = "Structure" }

hi "vimAugroup" { fg = "brightcyan" }
hi "vimMapRhs" { fg = "brightcyan" }
hi "vimNotation" { fg = "brightcyan" }
hi "vimFunc" { link = "Function" }
hi "vimFunction" { link = "Function" }
hi "vimUserFunc" { link = "Function" }

hi "xmlAttrib" { fg = "brightcyan" }
hi "xmlCdataStart" { fg = "brighterblack", attr = "bold" }
hi "xmlNamespace" { fg = "brightcyan" }
hi "xmlAttribPunct" { link = "Delimiter" }
hi "xmlCdata" { link = "Comment" }
hi "xmlCdataCdata" { link = "xmlCdataStart" }
hi "xmlCdataEnd" { link = "xmlCdataStart" }
hi "xmlEndTag" { link = "xmlTagName" }
hi "xmlProcessingDelim" { link = "Keyword" }
hi "xmlTagName" { link = "Keyword" }

hi "yamlBlockMappingKey" { fg = "brightcyan" }
hi "yamlBool" { link = "Keyword" }
hi "yamlDocumentStart" { link = "Keyword" }
hi "yamlKey" { fg = "brightcyan" }

------------------
-- Plugin Support
------------------

-- gitsigns
hi "GitSignsAdd" { fg = "green", bg = "darkblack" }
hi "GitSignsChange" { fg = "yellow", bg = "darkblack" }
hi "GitSignsChangeDelete" { fg = "red", bg = "darkblack" }
hi "GitSignsDelete" { fg = "red", bg = "darkblack" }

-- fugitive
hi "gitcommitDiscardedFile" { fg = "red" }
hi "gitcommitUntrackedFile" { fg = "red" }
hi "gitcommitSelectedFile" { fg = "green" }

-- nvim-treesitter
hi "TSAnnotation" { link = "Annotation" }
hi "TSConstBuiltin" { link = "Constant" }
hi "TSConstructor" { link = "Function" }
hi "TSEmphasis" { link = "Italic" }
hi "TSFuncBuiltin" { link = "Function" }
hi "TSFuncMacro" { link = "Function" }
hi "TSStringRegex" { link = "SpecialChar" }
hi "TSStrong" { link = "Bold" }
hi "TSStructure" { link = "Structure" }
hi "TSTagDelimiter" { link = "TSTag" }
hi "TSUnderline" { link = "Underline" }
hi "TSVariable" { link = "Variable" }
hi "TSVariableBuiltin" { link = "Keyword" }

-- termdebug
-- hi "debugPC" { fg = "cyan", bg = "darkgray" }
-- hi "debugBreakpoint" { fg = "red", bg = "darkgray" }

hi "TreesitterContext" { bg = "darkblack" }
