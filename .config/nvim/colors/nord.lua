local nvim_set_hl = vim.api.nvim_set_hl

vim.o.background = "dark"

local colors = {
    nord0        = { gui = "#2E3440", cterm = 0 },
    nord1        = { gui = "#3B4252", cterm = 0 },
    nord2        = { gui = "#434C5E", cterm = 2 },
    nord3        = { gui = "#4C566A", cterm = 8 },
    nord3_bright = { gui = "#616E88", cterm = 4 },
    nord4        = { gui = "#D8DEE9", cterm = 5 },
    nord5        = { gui = "#E5E9F0", cterm = 7 },
    nord6        = { gui = "#ECEFF4", cterm = 15 },
    nord7        = { gui = "#8FBCBB", cterm = 14 },
    nord8        = { gui = "#88C0D0", cterm = 6 },
    nord9        = { gui = "#81A1C1", cterm = 4 },
    nord10       = { gui = "#5E81AC", cterm = 12 },
    nord11       = { gui = "#BF616A", cterm = 1 },
    nord12       = { gui = "#D08770", cterm = 11 },
    nord13       = { gui = "#EBCB8B", cterm = 13 },
    nord14       = { gui = "#A3BE8C", cterm = 2 },
    nord15       = { gui = "#B48EAD", cterm = 5 },
}

vim.g.terminal_color_0 = colors.nord1.gui
vim.g.terminal_color_1 = colors.nord11.gui
vim.g.terminal_color_2 = colors.nord14.gui
vim.g.terminal_color_3 = colors.nord13.gui
vim.g.terminal_color_4 = colors.nord9.gui
vim.g.terminal_color_5 = colors.nord15.gui
vim.g.terminal_color_6 = colors.nord8.gui
vim.g.terminal_color_7 = colors.nord5.gui
vim.g.terminal_color_8 = colors.nord3.gui
vim.g.terminal_color_9 = colors.nord11.gui
vim.g.terminal_color_10 = colors.nord14.gui
vim.g.terminal_color_11 = colors.nord13.gui
vim.g.terminal_color_12 = colors.nord9.gui
vim.g.terminal_color_13 = colors.nord15.gui
vim.g.terminal_color_14 = colors.nord7.gui
vim.g.terminal_color_15 = colors.nord6.gui

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
hi "ColorColumn" { bg = "nord1" }
hi "Cursor" { fg = "nord0", bg = "nord4" }
hi "CursorLine" { bg = "nord1" }
hi "Error" { fg = "nord4", bg = "nord11" }
hi "iCursor" { fg = "nord0", bg = "nord4" }
hi "LineNr" { fg = "nord3" }
hi "MatchParen" { fg = "nord8", bg = "nord3" }
hi "NonText" { fg = "nord2" }
hi "Normal" { fg = "nord4", bg = "nord0" }
hi "Pmenu" { fg = "nord4", bg = "nord2" }
hi "PmenuSbar" { fg = "nord4", bg = "nord2" }
hi "PmenuSel" { fg = "nord8", bg = "nord3" }
hi "PmenuThumb" { fg = "nord8", bg = "nord3" }
hi "SpecialKey" { fg = "nord3" }
hi "SpellBad" { fg = "nord11", bg = "nord0", attr = "undercurl", sp = "nord11" }
hi "SpellCap" { fg = "nord13", bg = "nord0", attr = "undercurl", sp = "nord13" }
hi "SpellLocal" { fg = "nord5", bg = "nord0", attr = "undercurl", sp = "nord5" }
hi "SpellRare" { fg = "nord6", bg = "nord0", attr = "undercurl", sp = "nord6" }
hi "Visual" { bg = "nord2" }
hi "VisualNOS" { bg = "nord2" }

-- Neovim Support
hi "healthError" { fg = "nord11", bg = "nord1" }
hi "healthSuccess" { fg = "nord14", bg = "nord1" }
hi "healthWarning" { fg = "nord13", bg = "nord1" }
hi "TermCursorNC" { bg = "nord1" }

hi "DiagnosticWarn" { fg = "nord13" }
hi "DiagnosticError" { fg = "nord11" }
hi "DiagnosticInfo" { fg = "nord8" }
hi "DiagnosticHint" { fg = "nord10" }
hi "DiagnosticUnderlineWarn" { sp = "nord13", attr = "undercurl" }
hi "DiagnosticUnderlineError" { sp = "nord11", attr = "undercurl" }
hi "DiagnosticUnderlineInfo" { sp = "nord8", attr = "undercurl" }
hi "DiagnosticUnderlineHint" { sp = "nord10", attr = "undercurl" }

-- Neovim DocumentHighlight
hi "LspReferenceText" { bg = "nord3" }
hi "LspReferenceRead" { bg = "nord3" }
hi "LspReferenceWrite" { bg = "nord3" }

-- Neovim LspSignatureHelp
hi "LspSignatureActiveParameter" { fg = "nord8", attr = "underline" }

-- Gutter
hi "CursorColumn" { bg = "nord1" }
hi "CursorLineNr" { fg = "nord4", bg = "nord1" }
hi "Folded" { fg = "nord3", bg = "nord1", attr = "bold" }
hi "FoldColumn" { fg = "nord3", bg = "nord0" }
hi "SignColumn" { fg = "nord1", bg = "nord0" }

-- Navigation
hi "Directory" { fg = "nord8" }

-- Prompt/Status
hi "EndOfBuffer" { fg = "nord1" }
hi "ErrorMsg" { fg = "nord4", bg = "nord11" }
hi "ModeMsg" { fg = "nord4" }
hi "MoreMsg" { fg = "nord8" }
hi "Question" { fg = "nord4" }
hi "StatusLine" { fg = "nord8", bg = "nord3" }
hi "StatusLineNC" { fg = "nord4", bg = "nord1" }
hi "StatusLineTerm" { fg = "nord8", bg = "nord3" }
hi "StatusLineTermNC" { fg = "nord4", bg = "nord1" }
hi "WarningMsg" { fg = "nord0", bg = "nord13" }
hi "WildMenu" { fg = "nord8", bg = "nord1" }

-- Search
hi "IncSearch" { fg = "nord6", bg = "nord10", attr = "underline" }
hi "Search" { fg = "nord1", bg = "nord8" }

-- Tabs
hi "TabLine" { fg = "nord4", bg = "nord1" }
hi "TabLineFill" { fg = "nord4", bg = "nord1" }
hi "TabLineSel" { fg = "nord8", bg = "nord3" }

-- Window
hi "Title" { fg = "nord4" }

hi "VertSplit" { fg = "nord1", bg = "nord1" }

-- +----------------------+
-- + Language Base Groups +
-- +----------------------+
hi "Boolean" { fg = "nord9" }
hi "Character" { fg = "nord14" }
hi "Comment" { fg = "nord3_bright" }
hi "Conceal" {}
hi "Conditional" { fg = "nord9" }
hi "Constant" { fg = "nord4" }
hi "Decorator" { fg = "nord12" }
hi "Define" { fg = "nord9" }
hi "Delimiter" { fg = "nord6" }
hi "Exception" { fg = "nord9" }
hi "Float" { fg = "nord15" }
hi "Function" { fg = "nord8" }
hi "Identifier" { fg = "nord4" }
hi "Include" { fg = "nord9" }
hi "Keyword" { fg = "nord9" }
hi "Label" { fg = "nord9" }
hi "Number" { fg = "nord15" }
hi "Operator" { fg = "nord9" }
hi "PreProc" { fg = "nord9" }
hi "Repeat" { fg = "nord9" }
hi "Special" { fg = "nord4" }
hi "SpecialChar" { fg = "nord13" }
hi "SpecialComment" { fg = "nord8" }
hi "Statement" { fg = "nord9" }
hi "StorageClass" { fg = "nord9" }
hi "String" { fg = "nord14" }
hi "Structure" { fg = "nord9" }
hi "Tag" { fg = "nord4" }
hi "Todo" { fg = "nord13" }
hi "Type" { fg = "nord9" }
hi "Typedef" { fg = "nord9" }
hi "Annotation" { link = "Decorator" }
hi "Macro" { link = "Define" }
hi "PreCondit" { link = "PreProc" }
hi "Variable" { link = "Identifier" }

-------------
-- Languages
-------------
hi "asciidocAttributeEntry" { fg = "nord10" }
hi "asciidocAttributeList" { fg = "nord10" }
hi "asciidocAttributeRef" { fg = "nord10" }
hi "asciidocHLabel" { fg = "nord9" }
hi "asciidocListingBlock" { fg = "nord7" }
hi "asciidocMacroAttributes" { fg = "nord8" }
hi "asciidocOneLineTitle" { fg = "nord8" }
hi "asciidocPassthroughBlock" { fg = "nord9" }
hi "asciidocQuotedMonospaced" { fg = "nord7" }
hi "asciidocTriplePlusPassthrough" { fg = "nord7" }
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

hi "awkCharClass" { fg = "nord7" }
hi "awkPatterns" { fg = "nord9", attr = "bold" }
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

hi "cIncluded" { fg = "nord7" }
hi "cOperator" { link = "Operator" }
hi "cPreCondit" { link = "PreCondit" }
hi "cConstant" { link = "Type" }

hi "cmakeGeneratorExpression" { fg = "nord10" }

hi "csPreCondit" { link = "PreCondit" }
hi "csType" { link = "Type" }
hi "csXmlTag" { link = "SpecialComment" }

hi "cssAttributeSelector" { fg = "nord7" }
hi "cssDefinition" { fg = "nord7" }
hi "cssIdentifier" { fg = "nord7", attr = "underline" }
hi "cssStringQ" { fg = "nord7" }
hi "cssAttr" { link = "Keyword" }
hi "cssBraces" { link = "Delimiter" }
hi "cssClassName" { link = "cssDefinition" }
hi "cssColor" { link = "Number" }
hi "cssProp" { link = "cssDefinition" }
hi "cssPseudoClass" { link = "cssDefinition" }
hi "cssPseudoClassId" { link = "cssPseudoClass" }
hi "cssVendor" { link = "Keyword" }

hi "dosiniHeader" { fg = "nord8" }
hi "dosiniLabel" { link = "Type" }

hi "dtBooleanKey" { fg = "nord7" }
hi "dtExecKey" { fg = "nord7" }
hi "dtLocaleKey" { fg = "nord7" }
hi "dtNumericKey" { fg = "nord7" }
hi "dtTypeKey" { fg = "nord7" }
hi "dtDelim" { link = "Delimiter" }
hi "dtLocaleValue" { link = "Keyword" }
hi "dtTypeValue" { link = "Keyword" }

hi "DiffAdd" { fg = "nord14", bg = "nord0", attr = "inverse" }
hi "DiffChange" { fg = "nord13", bg = "nord0", attr = "inverse" }
hi "DiffDelete" { fg = "nord11", bg = "nord0", attr = "inverse" }
hi "DiffText" { fg = "nord9", bg = "nord0", attr = "inverse" }

-- Legacy groups for official git.vim and diff.vim syntax
hi "diffAdded" { link = "DiffAdd" }
hi "diffChanged" { link = "DiffChange" }
hi "diffRemoved" { link = "DiffDelete" }

hi "gitconfigVariable" { fg = "nord7" }
hi "gitrebaseFixup" { fg = "nord8" }
hi "gitrebaseExec" { fg = "nord8" }
hi "gitrebaseReword" { fg = "nord13" }

hi "goBuiltins" { fg = "nord7" }
hi "goConstants" { link = "Keyword" }

hi "haskellPreProc" { fg = "nord10" }
hi "haskellType" { fg = "nord7" }
hi "haskellPragma" { link = "haskellPreProc" }

hi "helpBar" { fg = "nord3" }
hi "helpHyperTextJump" { fg = "nord8", attr = "underline" }

hi "htmlArg" { fg = "nord7" }
hi "htmlLink" { fg = "nord4" }
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

hi "javaDocTags" { fg = "nord7" }
hi "javaCommentTitle" { link = "Comment" }
hi "javaScriptBraces" { link = "Delimiter" }
hi "javaScriptIdentifier" { link = "Keyword" }
hi "javaScriptNumber" { link = "Number" }

hi "jsGlobalNodeObjects" { fg = "nord8" }
hi "jsBrackets" { link = "Delimiter" }
hi "jsFuncCall" { link = "Function" }
hi "jsFuncParens" { link = "Delimiter" }
hi "jsThis" { link = "Keyword" }
hi "jsNoise" { link = "Delimiter" }
hi "jsPrototype" { link = "Keyword" }
hi "jsRegexpString" { link = "SpecialChar" }

hi "jsonKeyword" { fg = "nord7" }

hi "lessClass" { fg = "nord7" }
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

hi "markdownBlockquote" { fg = "nord7" }
hi "markdownCode" { fg = "nord7" }
hi "markdownCodeDelimiter" { fg = "nord7" }
hi "markdownFootnote" { fg = "nord7" }
hi "markdownId" { fg = "nord7" }
hi "markdownIdDeclaration" { fg = "nord7" }
hi "markdownH1" { fg = "nord8" }
hi "markdownLinkText" { fg = "nord8" }
hi "markdownUrl" { fg = "nord4" }
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

hi "pandocDefinitionBlockTerm" { fg = "nord7" }
hi "pandocTableDelims" { fg = "nord3" }
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

hi "perlPackageDecl" { fg = "nord7" }

hi "phpClasses" { fg = "nord7" }
hi "phpDocTags" { fg = "nord7" }
hi "phpDocCustomTags" { link = "phpDocTags" }
hi "phpMemberSelector" { link = "Keyword" }
hi "phpClass" { fg = "nord7" }
hi "phpClassImplements" { fg = "nord7", attr = "bold" }
hi "phpClassExtends" { link = "phpClass" }
hi "phpFunction" { link = "Function" }
hi "phpMethod" { link = "Function" }
hi "phpUseClass" { link = "phpClass" }

hi "podCmdText" { fg = "nord7" }
hi "podVerbatimLine" { fg = "nord4" }
hi "podFormat" { link = "Keyword" }

hi "pythonBuiltin" { link = "Type" }
hi "pythonEscape" { link = "SpecialChar" }

hi "rubyConstant" { fg = "nord7" }
hi "rubySymbol" { fg = "nord6", attr = "bold" }
hi "rubyAttribute" { link = "Identifier" }
hi "rubyBlockParameterList" { link = "Operator" }
hi "rubyInterpolationDelimiter" { link = "Keyword" }
hi "rubyKeywordAsMethod" { link = "Function" }
hi "rubyLocalVariableOrMethod" { link = "Function" }
hi "rubyPseudoVariable" { link = "Keyword" }
hi "rubyRegexp" { link = "SpecialChar" }

hi "rustAttribute" { fg = "nord10" }
hi "rustEnum" { fg = "nord7", attr = "bold" }
hi "rustMacro" { fg = "nord8", attr = "bold" }
hi "rustModPath" { fg = "nord7" }
hi "rustPanic" { fg = "nord9", attr = "bold" }
hi "rustTrait" { fg = "nord7" }
hi "rustCommentLineDoc" { link = "Comment" }
hi "rustDerive" { link = "rustAttribute" }
hi "rustEnumVariant" { link = "rustEnum" }
hi "rustEscape" { link = "SpecialChar" }
hi "rustQuestionMark" { link = "Keyword" }

hi "sassClass" { fg = "nord7" }
hi "sassId" { fg = "nord7", attr = "underline" }
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

hi "tsxAttrib" { fg = "nord7" }
hi "tsxEqual" { link = "Operator" }
hi "tsxIntrinsicTagName" { link = "htmlTag" }
hi "tsxTagName" { link = "tsxIntrinsicTagName" }

hi "typescriptBOMWindowMethod" { fg = "nord8" }
hi "typescriptClassName" { fg = "nord7" }
hi "typescriptDecorator" { fg = "nord12" }
hi "typescriptInterfaceName" { fg = "nord7", attr = "bold" }
hi "typescriptRegexpString" { fg = "nord13" }
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

hi "vimAugroup" { fg = "nord7" }
hi "vimMapRhs" { fg = "nord7" }
hi "vimNotation" { fg = "nord7" }
hi "vimFunc" { link = "Function" }
hi "vimFunction" { link = "Function" }
hi "vimUserFunc" { link = "Function" }

hi "xmlAttrib" { fg = "nord7" }
hi "xmlCdataStart" { fg = "nord3_bright", attr = "bold" }
hi "xmlNamespace" { fg = "nord7" }
hi "xmlAttribPunct" { link = "Delimiter" }
hi "xmlCdata" { link = "Comment" }
hi "xmlCdataCdata" { link = "xmlCdataStart" }
hi "xmlCdataEnd" { link = "xmlCdataStart" }
hi "xmlEndTag" { link = "xmlTagName" }
hi "xmlProcessingDelim" { link = "Keyword" }
hi "xmlTagName" { link = "Keyword" }

hi "yamlBlockMappingKey" { fg = "nord7" }
hi "yamlBool" { link = "Keyword" }
hi "yamlDocumentStart" { link = "Keyword" }
hi "yamlKey" { fg = "nord7" }

------------------
-- Plugin Support
------------------

-- gitsigns
hi "GitSignsAdd" { fg = "nord14" }
hi "GitSignsChange" { fg = "nord13" }
hi "GitSignsChangeDelete" { fg = "nord11" }
hi "GitSignsDelete" { fg = "nord11" }

-- fugitive
hi "gitcommitDiscardedFile" { fg = "nord11" }
hi "gitcommitUntrackedFile" { fg = "nord11" }
hi "gitcommitSelectedFile" { fg = "nord14" }

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

-- hi "TreesitterContext" { bg = "darkgray" }
