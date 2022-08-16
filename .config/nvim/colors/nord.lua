local nvim_set_hl = vim.api.nvim_set_hl

vim.o.background = "dark"

local background    = "#2E3440"
local darkblack     = "#373E4D"
local black         = "#3B4252"
local lightblack    = "#434C5E"
local brightblack   = "#4C566A"
local brighterblack = "#616E88"
local foreground    = "#D8DEE9"
local darkwhite     = "#AEB3BB"
local white         = "#E5E9F0"
local brightwhite   = "#ECEFF4"
local brightcyan    = "#8FBCBB"
local cyan          = "#88C0D0"
local blue          = "#81A1C1"
local darkblue      = "#5E81AC"
local red           = "#BF616A"
local orange        = "#D08770"
local yellow        = "#EBCB8B"
local green         = "#A3BE8C"
local magenta       = "#B48EAD"

vim.g.terminal_color_0 = black
vim.g.terminal_color_1 = red
vim.g.terminal_color_2 = green
vim.g.terminal_color_3 = yellow
vim.g.terminal_color_4 = blue
vim.g.terminal_color_5 = magenta
vim.g.terminal_color_6 = cyan
vim.g.terminal_color_7 = white
vim.g.terminal_color_8 = brightblack
vim.g.terminal_color_9 = red
vim.g.terminal_color_10 = green
vim.g.terminal_color_11 = yellow
vim.g.terminal_color_12 = blue
vim.g.terminal_color_13 = magenta
vim.g.terminal_color_14 = brightcyan
vim.g.terminal_color_15 = brightwhite

local highlights = {
    Normal = { fg = foreground, bg = background },

    Bold = { bold = true },
    Italic = {},
    Underlined = { underline = true },

    -- Editor
    ColorColumn = { bg = black },
    Cursor = { fg = background, bg = foreground },
    CursorLine = { bg = black },
    Error = { fg = foreground, bg = red },
    FloatBorder = { link = "Normal" },
    iCursor = { fg = background, bg = foreground },
    LineNr = { fg = brighterblack, bg = darkblack },
    MatchParen = { fg = cyan, bg = brightblack },
    NonText = { fg = lightblack },
    NormalFloat = { link = "Normal" },
    Pmenu = { fg = foreground, bg = lightblack },
    PmenuSbar = { fg = foreground, bg = lightblack },
    PmenuSel = { fg = cyan, bg = brightblack },
    PmenuThumb = { fg = cyan, bg = brightblack },
    SpecialKey = { fg = brightblack },
    SpellBad = { undercurl = true, sp = red },
    SpellCap = { undercurl = true, sp = yellow },
    SpellLocal = { undercurl = true, sp = white },
    SpellRare = { undercurl = true, sp = brightwhite },
    Visual = { bg = lightblack },
    VisualNOS = { bg = lightblack },

    -- Neovim Support
    healthError = { fg = red, bg = black },
    healthSuccess = { fg = green, bg = black },
    healthWarning = { fg = yellow, bg = black },
    TermCursorNC = { bg = black },

    DiagnosticWarn = { fg = yellow },
    DiagnosticError = { fg = red },
    DiagnosticInfo = { fg = cyan },
    DiagnosticHint = { fg = darkblue },
    DiagnosticSignWarn = { fg = yellow, bg = darkblack },
    DiagnosticSignError = { fg = red, bg = darkblack },
    DiagnosticSignInfo = { fg = cyan, bg = darkblack },
    DiagnosticSignHint = { fg = darkblue, bg = darkblack },
    DiagnosticUnderlineWarn = { sp = yellow, undercurl = true },
    DiagnosticUnderlineError = { sp = red, undercurl = true },
    DiagnosticUnderlineInfo = { sp = cyan, undercurl = true },
    DiagnosticUnderlineHint = { sp = darkblue, undercurl = true },

    -- Neovim DocumentHighlight
    LspReferenceText = { bg = brightblack },
    LspReferenceRead = { bg = brightblack },
    LspReferenceWrite = { bg = brightblack },

    -- Neovim LspSignatureHelp
    LspSignatureActiveParameter = { fg = cyan, underline = true },

    -- Gutter
    CursorColumn = { bg = darkblack },
    CursorLineNr = { fg = foreground, bg = darkblack },
    Folded = { fg = brightblack, bg = darkblack, bold = true },
    FoldColumn = { fg = brightblack, bg = darkblack },
    SignColumn = { fg = black, bg = darkblack },

    -- Navigation
    Directory = { fg = blue },

    -- Prompt/Status
    EndOfBuffer = { fg = black },
    ErrorMsg = { fg = foreground, bg = red },
    ModeMsg = { fg = foreground },
    MoreMsg = { fg = cyan },
    Question = { fg = foreground },
    StatusLine = { fg = foreground, bg = black },
    StatusLineNC = { fg = brighterblack, bg = darkblack },
    StatusLineTerm = { fg = foreground, bg = lightblack },
    StatusLineTermNC = { fg = brighterblack, bg = darkblack },
    WarningMsg = { fg = background, bg = yellow },
    WildMenu = { fg = cyan, bg = black },
    WinBar = { fg = brighterblack, bg = background },
    WinBarNC = { fg = brighterblack, bg = background },

    -- Search
    IncSearch = { fg = brightwhite, bg = darkblue, underline = true },
    Search = { fg = black, bg = cyan },

    -- Tabs
    TabLine = { fg = brighterblack, bg = black },
    TabLineFill = { fg = brighterblack, bg = darkblack },
    TabLineSel = { fg = foreground, bg = brightblack, bold = true },

    -- Window
    Title = { fg = foreground },

    WinSeparator = { fg = darkblack, bg = darkblack },

    QuickFixLine = { link = "Visual" },

    User1 = { fg = red, bg = black, bold = true },
    User2 = { fg = blue, bg = black, bold = true },
    User3 = { fg = foreground, bg = black, bold = true },
    User4 = { fg = black, bg = yellow },
    User5 = { fg = white, bg = lightblack },

    User8 = { fg = foreground, bg = brightblack },
    User9 = { fg = foreground, bg = darkblack },

    -----------------------
    -- Language Base Groups
    -----------------------
    Boolean = { fg = blue },
    Character = { fg = green },
    Comment = { fg = brighterblack },
    Conceal = {},
    Conditional = { fg = blue },
    Constant = { fg = foreground },
    Decorator = { fg = orange },
    Define = { fg = blue },
    Delimiter = { fg = brightwhite },
    Exception = { fg = blue },
    Float = { fg = magenta },
    Function = { fg = cyan },
    Identifier = { fg = foreground },
    Include = { fg = blue },
    Keyword = { fg = blue },
    Label = { fg = blue },
    Number = { fg = magenta },
    Operator = { fg = blue },
    PreProc = { fg = blue },
    Repeat = { fg = blue },
    Special = { fg = cyan },
    SpecialChar = { fg = yellow },
    SpecialComment = { fg = cyan },
    Statement = { fg = blue },
    StorageClass = { fg = blue },
    String = { fg = green },
    Structure = { fg = blue },
    Tag = { fg = foreground },
    Todo = { fg = yellow },
    Type = { fg = blue },
    Typedef = { fg = blue },
    Annotation = { link = "Decorator" },
    Macro = { link = "Define" },
    PreCondit = { link = "PreProc" },
    Variable = { link = "Identifier" },

    -------------
    -- Languages
    -------------
    asciidocAttributeEntry = { fg = darkblue },
    asciidocAttributeList = { fg = darkblue },
    asciidocAttributeRef = { fg = darkblue },
    asciidocHLabel = { fg = blue },
    asciidocListingBlock = { fg = brightcyan },
    asciidocMacroAttributes = { fg = cyan },
    asciidocOneLineTitle = { fg = cyan },
    asciidocPassthroughBlock = { fg = blue },
    asciidocQuotedMonospaced = { fg = brightcyan },
    asciidocTriplePlusPassthrough = { fg = brightcyan },
    asciidocAdmonition = { link = "Keyword" },
    asciidocBackslash = { link = "Keyword" },
    asciidocMacro = { link = "Keyword" },
    asciidocQuotedBold = { link = "Bold" },
    asciidocQuotedEmphasized = { link = "Italic" },
    asciidocQuotedMonospaced2 = { link = "asciidocQuotedMonospaced" },
    asciidocQuotedUnconstrainedBold = { link = "asciidocQuotedBold" },
    asciidocQuotedUnconstrainedEmphasized = { link = "asciidocQuotedEmphasized" },
    asciidocURL = { link = "markdownLinkText" },

    awkCharClass = { fg = brightcyan },
    awkPatterns = { fg = blue, bold = true },
    awkArrayElement = { link = "Identifier" },
    awkBoolLogic = { link = "Keyword" },
    awkBrktRegExp = { link = "SpecialChar" },
    awkComma = { link = "Delimiter" },
    awkExpression = { link = "Keyword" },
    awkFieldVars = { link = "Identifier" },
    awkLineSkip = { link = "Keyword" },
    awkOperator = { link = "Operator" },
    awkRegExp = { link = "SpecialChar" },
    awkSearch = { link = "Keyword" },
    awkSemicolon = { link = "Delimiter" },
    awkSpecialCharacter = { link = "SpecialChar" },
    awkSpecialPrintf = { link = "SpecialChar" },
    awkVariables = { link = "Identifier" },

    cIncluded = { fg = brightcyan },
    cOperator = { link = "Operator" },
    cPreCondit = { link = "PreCondit" },
    cConstant = { link = "Type" },

    cmakeGeneratorExpression = { fg = darkblue },

    csPreCondit = { link = "PreCondit" },
    csType = { link = "Type" },
    csXmlTag = { link = "SpecialComment" },

    cssAttributeSelector = { fg = brightcyan },
    cssDefinition = { fg = brightcyan },
    cssIdentifier = { fg = brightcyan, underline = true },
    cssStringQ = { fg = brightcyan },
    cssAttr = { link = "Keyword" },
    cssBraces = { link = "Delimiter" },
    cssClassName = { link = "cssDefinition" },
    cssColor = { link = "Number" },
    cssProp = { link = "cssDefinition" },
    cssPseudoClass = { link = "cssDefinition" },
    cssPseudoClassId = { link = "cssPseudoClass" },
    cssVendor = { link = "Keyword" },

    dosiniHeader = { fg = cyan },
    dosiniLabel = { link = "Type" },

    dtBooleanKey = { fg = brightcyan },
    dtExecKey = { fg = brightcyan },
    dtLocaleKey = { fg = brightcyan },
    dtNumericKey = { fg = brightcyan },
    dtTypeKey = { fg = brightcyan },
    dtDelim = { link = "Delimiter" },
    dtLocaleValue = { link = "Keyword" },
    dtTypeValue = { link = "Keyword" },

    DiffAdd = { fg = green, bg = background },
    DiffChange = { fg = yellow, bg = background },
    DiffDelete = { fg = red, bg = background },
    DiffText = { fg = blue, bg = background },

    -- Legacy groups for official git.vim and diff.vim syntax
    diffAdded = { link = "DiffAdd" },
    diffChanged = { link = "DiffChange" },
    diffRemoved = { link = "DiffDelete" },

    gitconfigVariable = { fg = brightcyan },
    gitrebaseFixup = { fg = cyan },
    gitrebaseExec = { fg = cyan },
    gitrebaseReword = { fg = yellow },

    goBuiltins = { fg = brightcyan },
    goConstants = { link = "Keyword" },

    haskellPreProc = { fg = darkblue },
    haskellType = { fg = brightcyan },
    haskellPragma = { link = "haskellPreProc" },

    helpBar = { fg = brightblack },
    helpHyperTextJump = { fg = cyan, underline = true },

    htmlArg = { fg = brightcyan },
    htmlLink = { fg = foreground },
    htmlBold = { link = "Bold" },
    htmlEndTag = { link = "htmlTag" },
    htmlItalic = { link = "Italic" },
    htmlH1 = { link = "markdownH1" },
    htmlH2 = { link = "markdownH1" },
    htmlH3 = { link = "markdownH1" },
    htmlH4 = { link = "markdownH1" },
    htmlH5 = { link = "markdownH1" },
    htmlH6 = { link = "markdownH1" },
    htmlSpecialChar = { link = "SpecialChar" },
    htmlTag = { link = "Keyword" },
    htmlTagN = { link = "htmlTag" },

    javaDocTags = { fg = brightcyan },
    javaCommentTitle = { link = "Comment" },
    javaScriptBraces = { link = "Delimiter" },
    javaScriptIdentifier = { link = "Keyword" },
    javaScriptNumber = { link = "Number" },

    jsGlobalNodeObjects = { fg = cyan },
    jsBrackets = { link = "Delimiter" },
    jsFuncCall = { link = "Function" },
    jsFuncParens = { link = "Delimiter" },
    jsThis = { link = "Keyword" },
    jsNoise = { link = "Delimiter" },
    jsPrototype = { link = "Keyword" },
    jsRegexpString = { link = "SpecialChar" },

    jsonKeyword = { fg = brightcyan },

    lessClass = { fg = brightcyan },
    lessAmpersand = { link = "Keyword" },
    lessCssAttribute = { link = "Delimiter" },
    lessFunction = { link = "Function" },
    cssSelectorOp = { link = "Keyword" },

    lispAtomBarSymbol = { link = "SpecialChar" },
    lispAtomList = { link = "SpecialChar" },
    lispAtomMark = { link = "Keyword" },
    lispBarSymbol = { link = "SpecialChar" },
    lispFunc = { link = "Function" },

    luaFunc = { link = "Function" },

    markdownBlockquote = { fg = brightcyan },
    markdownCode = { fg = brightcyan },
    markdownCodeDelimiter = { fg = brightcyan },
    markdownFootnote = { fg = brightcyan },
    markdownId = { fg = brightcyan },
    markdownIdDeclaration = { fg = brightcyan },
    markdownH1 = { fg = cyan },
    markdownLinkText = { fg = cyan },
    markdownUrl = { fg = foreground },
    markdownBold = { link = "Bold" },
    markdownBoldDelimiter = { link = "Keyword" },
    markdownFootnoteDefinition = { link = "markdownFootnote" },
    markdownH2 = { link = "markdownH1" },
    markdownH3 = { link = "markdownH1" },
    markdownH4 = { link = "markdownH1" },
    markdownH5 = { link = "markdownH1" },
    markdownH6 = { link = "markdownH1" },
    markdownIdDelimiter = { link = "Keyword" },
    markdownItalic = { link = "Italic" },
    markdownItalicDelimiter = { link = "Keyword" },
    markdownLinkDelimiter = { link = "Keyword" },
    markdownLinkTextDelimiter = { link = "Keyword" },
    markdownListMarker = { link = "Keyword" },
    markdownRule = { link = "Keyword" },
    markdownHeadingDelimiter = { link = "Keyword" },

    pandocDefinitionBlockTerm = { fg = brightcyan },
    pandocTableDelims = { fg = brightblack },
    pandocAtxHeader = { link = "markdownH1" },
    pandocBlockQuote = { link = "markdownBlockquote" },
    pandocCiteAnchor = { link = "Operator" },
    pandocCiteKey = { link = "pandocReferenceLabel" },
    pandocDefinitionBlockMark = { link = "Operator" },
    pandocEmphasis = { link = "markdownItalic" },
    pandocFootnoteID = { link = "pandocReferenceLabel" },
    pandocFootnoteIDHead = { link = "markdownLinkDelimiter" },
    pandocFootnoteIDTail = { link = "pandocFootnoteIDHead" },
    pandocGridTableDelims = { link = "pandocTableDelims" },
    pandocGridTableHeader = { link = "pandocTableDelims" },
    pandocOperator = { link = "Operator" },
    pandocPipeTableDelims = { link = "pandocTableDelims" },
    pandocReferenceDefinition = { link = "pandocReferenceLabel" },
    pandocReferenceLabel = { link = "markdownLinkText" },
    pandocReferenceURL = { link = "markdownUrl" },
    pandocSimpleTableHeader = { link = "pandocAtxHeader" },
    pandocStrong = { link = "markdownBold" },
    pandocTableHeaderWord = { link = "pandocAtxHeader" },
    pandocUListItemBullet = { link = "Operator" },

    perlPackageDecl = { fg = brightcyan },

    phpClasses = { fg = brightcyan },
    phpDocTags = { fg = brightcyan },
    phpDocCustomTags = { link = "phpDocTags" },
    phpMemberSelector = { link = "Keyword" },
    phpClass = { fg = brightcyan },
    phpClassImplements = { fg = brightcyan, bold = true },
    phpClassExtends = { link = "phpClass" },
    phpFunction = { link = "Function" },
    phpMethod = { link = "Function" },
    phpUseClass = { link = "phpClass" },

    podCmdText = { fg = brightcyan },
    podVerbatimLine = { fg = foreground },
    podFormat = { link = "Keyword" },

    pythonBuiltin = { link = "Type" },
    pythonEscape = { link = "SpecialChar" },

    rubyConstant = { fg = brightcyan },
    rubySymbol = { fg = brightwhite, bold = true },
    rubyAttribute = { link = "Identifier" },
    rubyBlockParameterList = { link = "Operator" },
    rubyInterpolationDelimiter = { link = "Keyword" },
    rubyKeywordAsMethod = { link = "Function" },
    rubyLocalVariableOrMethod = { link = "Function" },
    rubyPseudoVariable = { link = "Keyword" },
    rubyRegexp = { link = "SpecialChar" },

    rustAttribute = { fg = darkblue },
    rustEnum = { fg = brightcyan, bold = true },
    rustMacro = { fg = cyan, bold = true },
    rustModPath = { fg = brightcyan },
    rustPanic = { fg = blue, bold = true },
    rustTrait = { fg = brightcyan },
    rustCommentLineDoc = { link = "Comment" },
    rustDerive = { link = "rustAttribute" },
    rustEnumVariant = { link = "rustEnum" },
    rustEscape = { link = "SpecialChar" },
    rustQuestionMark = { link = "Keyword" },

    sassClass = { fg = brightcyan },
    sassId = { fg = brightcyan, underline = true },
    sassAmpersand = { link = "Keyword" },
    sassClassChar = { link = "Delimiter" },
    sassControl = { link = "Keyword" },
    sassControlLine = { link = "Keyword" },
    sassExtend = { link = "Keyword" },
    sassFor = { link = "Keyword" },
    sassFunctionDecl = { link = "Keyword" },
    sassFunctionName = { link = "Function" },
    sassidChar = { link = "sassId" },
    sassInclude = { link = "SpecialChar" },
    sassMixinName = { link = "Function" },
    sassMixing = { link = "SpecialChar" },
    sassReturn = { link = "Keyword" },

    shCmdParenRegion = { link = "Delimiter" },
    shCmdSubRegion = { link = "Delimiter" },
    shDerefSimple = { link = "Identifier" },
    shDerefVar = { link = "Identifier" },

    sqlKeyword = { link = "Keyword" },
    sqlSpecial = { link = "Keyword" },

    tsxAttrib = { fg = brightcyan },
    tsxEqual = { link = "Operator" },
    tsxIntrinsicTagName = { link = "htmlTag" },
    tsxTagName = { link = "tsxIntrinsicTagName" },

    typescriptBOMWindowMethod = { fg = cyan },
    typescriptClassName = { fg = brightcyan },
    typescriptDecorator = { fg = orange },
    typescriptInterfaceName = { fg = brightcyan, bold = true },
    typescriptRegexpString = { fg = yellow },
    typescriptOperator = { link = "Operator" },
    typescriptBinaryOp = { link = "Operator" },
    typescriptAssign = { link = "Operator" },
    typescriptMember = { link = "Identifier" },
    typescriptDOMStorageMethod = { link = "Identifier" },
    typescriptArrowFuncArg = { link = "Identifier" },
    typescriptGlobal = { link = "typescriptClassName" },
    typescriptBOMWindowProp = { link = "Function" },
    typescriptArrowFuncDef = { link = "Function" },
    typescriptAliasDeclaration = { link = "Function" },
    typescriptPredefinedType = { link = "Type" },
    typescriptTypeReference = { link = "typescriptClassName" },
    typescriptTypeAnnotation = { link = "Structure" },
    typescriptDocNamedParamType = { link = "SpecialComment" },
    typescriptDocNotation = { link = "Keyword" },
    typescriptDocTags = { link = "Keyword" },
    typescriptImport = { link = "Keyword" },
    typescriptExport = { link = "Keyword" },
    typescriptTry = { link = "Keyword" },
    typescriptVariable = { link = "Keyword" },
    typescriptBraces = { link = "Normal" },
    typescriptObjectLabel = { link = "Normal" },
    typescriptCall = { link = "Normal" },
    typescriptClassHeritage = { link = "typescriptClassName" },
    typescriptFuncTypeArrow = { link = "Structure" },
    typescriptMemberOptionality = { link = "Structure" },
    typescriptNodeGlobal = { link = "typescriptGlobal" },
    typescriptTypeBrackets = { link = "Structure" },

    vimAugroup = { fg = brightcyan },
    vimMapRhs = { fg = brightcyan },
    vimNotation = { fg = brightcyan },
    vimFunc = { link = "Function" },
    vimFunction = { link = "Function" },
    vimUserFunc = { link = "Function" },

    xmlAttrib = { fg = brightcyan },
    xmlCdataStart = { fg = brighterblack, bold = true },
    xmlNamespace = { fg = brightcyan },
    xmlAttribPunct = { link = "Delimiter" },
    xmlCdata = { link = "Comment" },
    xmlCdataCdata = { link = "xmlCdataStart" },
    xmlCdataEnd = { link = "xmlCdataStart" },
    xmlEndTag = { link = "xmlTagName" },
    xmlProcessingDelim = { link = "Keyword" },
    xmlTagName = { link = "Keyword" },

    yamlBlockMappingKey = { fg = brightcyan },
    yamlBool = { link = "Keyword" },
    yamlDocumentStart = { link = "Keyword" },
    yamlKey = { fg = brightcyan },

    ------------------
    -- Plugin Support
    ------------------

    -- gitsigns
    GitSignsAdd = { fg = green, bg = darkblack },
    GitSignsChange = { fg = yellow, bg = darkblack },
    GitSignsChangeDelete = { fg = red, bg = darkblack },
    GitSignsDelete = { fg = red, bg = darkblack },
    GitSignsCurrentLineBlame = { link = "Comment" },

    -- fugitive
    gitcommitDiscardedFile = { fg = red },
    gitcommitUntrackedFile = { fg = red },
    gitcommitSelectedFile = { fg = green },

    -- nvim-treesitter
    TSAnnotation = { link = "Annotation" },
    TSConstBuiltin = { link = "Constant" },
    TSConstructor = { link = "Function" },
    TSEmphasis = { link = "Italic" },
    TSFuncBuiltin = { link = "Function" },
    TSFuncMacro = { link = "Function" },
    TSStringRegex = { link = "SpecialChar" },
    TSStrong = { link = "Bold" },
    TSStructure = { link = "Structure" },
    TSTagDelimiter = { link = "TSTag" },
    TSUnderline = { link = "Underline" },
    TSVariable = { link = "Variable" },
    TSVariableBuiltin = { link = "Keyword" },

    -- termdebug
    --  debugPC = { fg = cyan, bg = darkgray },
    --  debugBreakpoint = { fg = red, bg = darkgray },

    TreesitterContext = { bg = darkblack },

    Sneak = { bg = yellow, fg = black },

    TelescopeMatching = { fg = yellow },
}

for group, opts in pairs(highlights) do
    nvim_set_hl(0, group, opts)
end
