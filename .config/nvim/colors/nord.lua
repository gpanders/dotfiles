local nvim_set_hl = vim.api.nvim_set_hl

local background = "#2e3440"
local foreground = "#eceff4"

-- (nord1) Used for elevated, more prominent or focused UI elements like, status bars
-- and text editor gutters panels, modals and floating popups like
-- notifications or auto completion user interaction/form components like
-- buttons, text/select fields or checkboxes. It also works fine for more
-- inconspicuous and passive elements like borders or as dropshadow between
-- different components.
local black = "#3b4252"

-- (nord2) Used to colorize the currently active text editor line as well as
-- selection and text highlighting color. It can also be used as an brighter
-- variant for the same target elements like nord1.
local lightblack = "#434c5e"

-- (nord3) Used for UI elements like indent- and wrap guide marker.
local brightblack = "#4c566a"

-- Higher contrast variant of nord3 used for comments and
-- invisible/non-printable characters.
local brighterblack = "#616e88"

-- (nord4) Used for UI elements like the text editor caret. In the context of syntax
-- highlighting it is used as text color for variables, constants, attributes
-- and fields.
local darkwhite = "#d8dee9"

-- (nord5) Used for more subtle/inconspicuous UI text elements that do not need so much
-- visual attention. Other use cases are also state animations like a more
-- brighter text color when a button is hovered, active or focused.
local white = "#e5e9f0"

-- (nord6) Used for elevated UI text elements that require more visual attention. In
-- the context of syntax highlighting it is used as text color for plain text
-- as well as reserved and structuring syntax characters like curly- and square
-- brackets.
local brightwhite = "#eceff4"

-- (nord7) Used for UI elements that should, next to the primary accent color nord8,
-- stand out and get more visual attention. In the context of syntax
-- highlighting it is used for classes, types and primitives.
local brightcyan = "#8fbcbb"

-- (nord8) Used for primary UI elements with main usage purposes that require
-- the most visual attention. In the context of syntax highlighting it is used
-- for declarations, calls and execution statements of functions, methods and
-- routines.
local cyan = "#88c0d0"

-- (nord9) Used for secondary UI elements that also require more visual attention than
-- other elements. In the context of syntax highlighting it is used for
-- language specific, syntactic and reserved keywords as well as support
-- characters, operators, tags, units, and punctuation like (semi)colons,
-- points and commas
local blue = "#81a1c1"

-- (nord10) Used for tertiary UI elements that require more visual attention than
-- default elements. In the context of syntax highlighting it is used for
-- pragmas, comment keywords and pre-processor statements.
local darkblue = "#5e81ac"

-- (nord11) Used for UI elements that are rendering error states like linter
-- markers and the highlighting of Git diff deletions. In the context of syntax
-- highlighting it is used to override the highlighting of syntax elements that
-- are detected as errors.
local red = "#bf616a"

-- (nord12) Rarely used for UI elements, but it may indicate a more advanced or
-- dangerous functionality. In the context of syntax highlighting it is used
-- for special syntax elements like annotations and decorators.
local orange = "#d08770"

-- (nord13) Used for UI elements that are rendering warning states like linter
-- markers and the highlighting of Git diff modifications. In the context of
-- syntax highlighting it is used to override the highlighting of syntax
-- elements that are detected as warnings as well as escape characters and
-- within regular expressions.
local yellow = "#ebcb8b"

-- (nord14) Used for UI elements that are rendering success states and
-- visualizations and the highlighting of Git diff additions. In the context of
-- syntax highlighting it is used as main color for strings of any type like
-- double/single quoted or interpolated.
local green = "#a3be8c"

-- (nord15) Rarely used for UI elements, but it may indicate a more uncommon
-- functionality. In the context of syntax highlighting it is used as main
-- color for numbers of any type like integers and floating point numbers.
local magenta = "#b48ead"

vim.g.terminal_color_0 = background
vim.g.terminal_color_1 = red
vim.g.terminal_color_2 = green
vim.g.terminal_color_3 = yellow
vim.g.terminal_color_4 = blue
vim.g.terminal_color_5 = magenta
vim.g.terminal_color_6 = cyan
vim.g.terminal_color_7 = foreground
vim.g.terminal_color_8 = black
vim.g.terminal_color_9 = red
vim.g.terminal_color_10 = green
vim.g.terminal_color_11 = yellow
vim.g.terminal_color_12 = blue
vim.g.terminal_color_13 = magenta
vim.g.terminal_color_14 = cyan
vim.g.terminal_color_15 = brightwhite

local highlights = {
    Normal = {},

    Bold = { bold = true },
    Italic = { italic = true },
    Underlined = { underline = true },

    -- Editor
    Attribute = { fg = darkwhite },
    ColorColumn = { bg = black },
    CursorLine = { bg = black },
    Error = { fg = foreground, bg = red },
    FloatBorder = { link = "Normal" },
    LineNr = { fg = brightblack, bg = background },
    MatchParen = { bg = brightblack, bold = true },
    NonText = { fg = brightblack },
    NormalFloat = { link = "Normal" },
    FloatShadow = { bg = black, blend = 80 },
    FloatShadowThrough = { bg = black, blend = 100 },
    Pmenu = { fg = white, bg = black },
    PmenuSbar = { link = "Pmenu" },
    PmenuSel = { fg = cyan, bg = lightblack },
    PmenuThumb = { bg = brightblack },
    SpecialKey = { fg = brightblack },
    SpellBad = { undercurl = true, sp = red },
    SpellCap = { undercurl = true, sp = yellow },
    SpellLocal = { undercurl = true, sp = yellow },
    SpellRare = { undercurl = true, sp = yellow },
    Visual = { bg = lightblack },
    VisualNOS = { bg = lightblack },

    DiagnosticOk = { fg = green },
    DiagnosticWarn = { fg = yellow },
    DiagnosticError = { fg = red },
    DiagnosticInfo = { fg = blue },
    DiagnosticHint = { fg = darkblue },
    DiagnosticUnderlineOk = { sp = green, undercurl = true },
    DiagnosticUnderlineWarn = { sp = yellow, undercurl = true },
    DiagnosticUnderlineError = { sp = red, undercurl = true },
    DiagnosticUnderlineInfo = { sp = blue, undercurl = true },
    DiagnosticUnderlineHint = { sp = darkblue, undercurl = true },
    DiagnosticDeprecated = { sp = red, strikethrough = true },

    -- LSP
    LspReferenceText = { bg = brightblack },
    LspReferenceRead = { bg = brightblack },
    LspReferenceWrite = { bg = brightblack },
    LspSignatureActiveParameter = { underline = true, bold = true, sp = foreground },
    LspCodeLens = { link = "Comment" },
    LspCodeLensSeparator = { link = "Comment" },
    LspInlayHint = { fg = brightblack },
    ["@lsp.type.namespace"] = { link = "Identifier" },
    ["@lsp.type.operator"] = { link = "Operator" },

    -- Gutter
    CursorColumn = { bg = black },
    CursorLineNr = { fg = foreground, bg = background },
    Folded = { fg = brightblack },
    FoldColumn = { fg = brightblack, bg = background },
    SignColumn = { fg = black, bg = background },

    -- Navigation
    Directory = { fg = blue },

    -- Prompt/Status
    EndOfBuffer = { fg = black },
    ErrorMsg = { fg = foreground, bg = red },
    ModeMsg = { fg = foreground },
    MoreMsg = { fg = cyan },
    Question = { fg = foreground },
    StatusLine = { fg = foreground, bg = black },
    StatusLineNC = { fg = brighterblack, bg = black },
    StatusLineTerm = { fg = background, bg = green, bold = true },
    StatusLineTermNC = { fg = background, bg = green },
    WarningMsg = { fg = background, bg = yellow },
    WildMenu = { fg = cyan, bg = black },
    WinBar = { fg = brightblack, bg = background },
    WinBarNC = { fg = brightblack, bg = background },

    -- Search
    IncSearch = { fg = black, bg = cyan, underline = true },
    CurSearch = { fg = black, bg = cyan },
    Search = { fg = brightwhite, bg = darkblue },

    -- Tabs
    TabLine = { fg = brightblack, bg = black },
    TabLineFill = { fg = brightblack, bg = black },
    TabLineSel = { fg = foreground, bg = brightblack, bold = true },

    -- Window
    Title = { fg = cyan },

    WinSeparator = { fg = black },

    QuickFixLine = { link = "Visual" },

    User1 = { fg = red, bg = black, bold = true },
    User2 = { fg = blue, bg = black, bold = true },
    User3 = { fg = foreground, bg = black, bold = true },
    User4 = { fg = black, bg = yellow },

    User8 = { fg = foreground, bg = brightblack },
    User9 = { fg = foreground, bg = black },

    -----------------------
    -- Language Base Groups
    -----------------------
    Boolean = { link = "Keyword" },
    Character = { fg = green },
    Comment = { fg = brighterblack },
    Conceal = {},
    Conditional = { link = "Keyword" },
    Constant = { fg = darkwhite },
    Decorator = { fg = orange },
    Define = { link = "PreProc" },
    Delimiter = { fg = brightwhite },
    Exception = { fg = blue },
    Float = { fg = magenta },
    Function = { fg = cyan },
    Identifier = { fg = darkwhite },
    Include = { link = "PreProc" },
    Keyword = { fg = blue },
    Label = { fg = darkblue },
    Number = { fg = magenta },
    Operator = { fg = darkwhite },
    PreProc = { fg = darkblue },
    Repeat = { link = "Keyword" },
    Special = { fg = yellow },
    SpecialChar = { fg = yellow },
    SpecialComment = { fg = cyan },
    Statement = { fg = blue },
    StorageClass = { fg = blue },
    String = { fg = green },
    Structure = { link = "Type" },
    Tag = { fg = blue },
    Todo = { fg = yellow },
    Type = { fg = brightcyan },
    Typedef = { fg = brightcyan },
    Annotation = { link = "Decorator" },
    Macro = { link = "Define" },
    PreCondit = { link = "PreProc" },
    Variable = { link = "Identifier" },
    Constructor = {},

    ---------------
    -- Tree-sitter
    ---------------
    ["@attribute"] = { link = "Attribute" },
    ["@constant.builtin"] = { link = "Constant" },
    ["@constructor"] = { link = "Function" },
    ["@constant.macro"] = { link = "Macro" },
    ["@function.builtin"] = { link = "Function" },
    ["@variable.builtin"] = { link = "Keyword" },
    ["@markup.link"] = { fg = blue, underline = true },
    ["@module"] = { link = "Identifier" },
    ["@punctuation.special"] = { fg = brightwhite },
    ["@type.builtin"] = { link = "@type" },

    -------------
    -- Languages
    -------------
    asciidocAttributeEntry = { link = "Attribute" },
    asciidocAttributeList = { link = "Attribute" },
    asciidocAttributeRef = { link = "Attribute" },
    asciidocHLabel = { fg = blue },
    asciidocListingBlock = { fg = brightcyan },
    asciidocMacroAttributes = { link = "Attribute" },
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

    cIncluded = { link = "Include" },
    cOperator = { link = "Operator" },
    cPreCondit = { link = "PreCondit" },
    cConstant = { link = "Constant" },
    ["@lsp.type.macro.c"] = {},
    ["@keyword.conditional.ternary.c"] = { link = "Operator" },

    cmakeGeneratorExpression = { fg = darkblue },

    csPreCondit = { link = "PreCondit" },
    csType = { link = "Type" },
    csXmlTag = { link = "SpecialComment" },

    cssAttributeSelector = { link = "Attribute" },
    cssDefinition = { fg = brightcyan },
    cssIdentifier = { fg = brightcyan, underline = true },
    cssStringQ = { fg = brightcyan },
    cssAttr = { link = "Keyword" },
    cssBraces = { link = "Delimiter" },
    cssClassName = { link = "Type", },
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

    Added = { link = "DiffAdd" },
    Changed = { link = "DiffChange" },
    Removed = { link = "DiffDelete" },

    -- Elixir
    ["@attribute.elixir"] = { link = "Decorator" },

    gitconfigVariable = { fg = brightcyan },
    gitrebaseFixup = { fg = cyan },
    gitrebaseExec = { fg = cyan },
    gitrebaseReword = { fg = yellow },

    goBuiltins = { fg = brightcyan },
    goConstants = { link = "Keyword" },

    haskellPreProc = { fg = darkblue },
    haskellType = { fg = brightcyan },
    haskellPragma = { link = "haskellPreProc" },

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
    jsonCommentError = { link = "Comment" },

    lessClass = { link = "Type", },
    lessAmpersand = { link = "Keyword" },
    lessCssAttribute = { link = "Attribute" },
    lessFunction = { link = "Function" },
    cssSelectorOp = { link = "Keyword" },

    lispAtomBarSymbol = { link = "SpecialChar" },
    lispAtomList = { link = "SpecialChar" },
    lispAtomMark = { link = "Keyword" },
    lispBarSymbol = { link = "SpecialChar" },
    lispFunc = { link = "Function" },

    luaFunc = { link = "Function" },

    ["@markup.link.label.markdown_inline"] = { link = "@markup.link.label" },
    ["@markup.link.markdown_inline"] = { link = "@markup.link" },
    ["@markup.raw.markdown_inline"] = { bg = black },

    pandocDefinitionBlockTerm = { fg = brightcyan },
    pandocTableDelims = { fg = brighterblack },
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
    phpClass = { link = "Type", },
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
    rubyAttribute = { link = "Attribute" },
    rubyBlockParameterList = { link = "Operator" },
    rubyInterpolationDelimiter = { link = "Keyword" },
    rubyKeywordAsMethod = { link = "Function" },
    rubyLocalVariableOrMethod = { link = "Function" },
    rubyPseudoVariable = { link = "Keyword" },
    rubyRegexp = { link = "SpecialChar" },

    rustAttribute = { link = "Attribute" },
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
    ["@module.rust"] = { link = "Identifier" },

    sassClass = { link = "Type", },
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

    swiftAttribute = { link = "Decorator" },
    swiftDefinitionModifier = { link = "Keyword" },
    swiftVarDefinition = { link = "Keyword" },
    swiftFuncDefinition = { link = "Keyword" },
    swiftTypeDefinition = { link = "Keyword" },

    tsxAttrib = { fg = brightcyan },
    tsxEqual = { link = "Operator" },
    tsxIntrinsicTagName = { link = "htmlTag" },
    tsxTagName = { link = "tsxIntrinsicTagName" },

    typescriptBOMWindowMethod = { fg = cyan },
    typescriptClassName = { link = "Type", },
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

    ["@variable.parameter.vimdoc"] = { fg = cyan },

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

    ["@attribute.zig"] = { link = "Keyword" },

    ------------------
    -- Plugin Support
    ------------------

    -- fugitive
    gitcommitDiscardedFile = { fg = red },
    gitcommitUntrackedFile = { fg = red },
    gitcommitSelectedFile = { fg = green },

    -- mini.nvim
    MiniIndentscopeSymbol = { fg = brightblack },

    -- nvim-dap
    DapStoppedLine = { bg = brighterblack, underline = true },
    DapStoppedSign = { fg = black, bg = white },
    DapBreakpoint = { bg = black },

    -- nvim-treesitter-context
    TreesitterContext = { bg = black },
}

for group, opts in pairs(highlights) do
    nvim_set_hl(0, group, opts)
end

vim.g.colors_name = 'nord'
