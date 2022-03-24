(import-macros {: make-colors} :colors)

(when vim.g.colors_name
  (exec "hi clear"))

(set vim.g.colors_name :base16-eighties)

(make-colors
  {:black       {:gui "#2d2d2d" :cterm 0}
   :red         {:gui "#f2777a" :cterm 1}
   :green       {:gui "#99cc99" :cterm 2}
   :yellow      {:gui "#ffcc66" :cterm 3}
   :blue        {:gui "#6699cc" :cterm 4}
   :magenta     {:gui "#cc99cc" :cterm 5}
   :cyan        {:gui "#66cccc" :cterm 6}
   :normal      {:gui "#d3d0c8" :cterm 7}
   :lightgray   {:gui "#747369" :cterm 8}
   :orange      {:gui "#f99157" :cterm 9}
   :darkgray    {:gui "#393939" :cterm 10}
   :gray        {:gui "#515151" :cterm 11}
   :lightergray {:gui "#a09f93" :cterm 12}
   :white       {:gui "#e8e6df" :cterm 13}
   :darkorange  {:gui "#d27b53" :cterm 14}
   :brwhite     {:gui "#f2f0ec" :cterm 15}}

  Normal {:fg "normal" :bg "black"}

  ; UI Highlights (:h highlight-groups)
  ColorColumn {:bg "darkgray"}
  Conceal {:fg "blue"}
  Cursor {:fg "black" :bg "normal"}
  CursorColumn {:bg "darkgray"}
  CursorLine {:bg "darkgray"}
  CursorLineNr {:fg "lightergray" :bg "darkgray"}
  DiffAdd {:fg "green"}
  DiffChange {:fg "lightgray"}
  DiffDelete {:fg "red"}
  DiffText {:fg "blue"}
  Directory {:fg "blue"}
  ErrorMsg {:fg "red"}
  FloatBorder {:bg "black"}
  FoldColumn {:fg "cyan" :bg "darkgray"}
  Folded {:fg "lightgray" :bg "darkgray"}
  IncSearch {:fg "darkgray" :bg "orange"}
  LineNr {:fg "lightgray" :bg "darkgray"}
  MatchParen {:bg "lightgray"}
  MatchWord {:bg "darkgray"}
  ModeMsg {:fg "green"}
  MoreMsg {:fg "green"}
  NonText {:fg "lightgray"}
  NormalFloat {:bg "darkgray"}
  PMenu {:bg "darkgray"}
  PMenuSel {:fg "darkgray" :bg "normal"}
  Question {:fg "blue"}
  QuickFixLine {:bg "darkgray"}
  Search {:fg "darkgray" :bg "yellow"}
  SignColumn {:fg "lightgray" :bg "darkgray"}
  SpecialKey {:fg "green"}
  SpellBad {:attr "undercurl" :guisp "red"}
  SpellCap {:attr "undercurl" :guisp "magenta"}
  SpellLocal {:attr "undercurl" :guisp "blue"}
  SpellRare {:attr "undercurl" :guisp "cyan"}
  StatusLine {:fg "lightergray" :bg "gray"}
  StatusLineNC {:fg "lightgray" :bg "darkgray"}
  TabLine {:fg "lightgray" :bg "darkgray"}
  TabLineFill {:fg "lightgray" :bg "darkgray"}
  TabLineSel {:fg "green" :bg "darkgray"}
  Title {:fg "blue"}
  VertSplit {:fg "gray" :bg "gray"}
  Visual {:bg "gray"}
  VisualNOS {:fg "red"}
  WarningMsg {:fg "yellow"}
  WildMenu {:fg "brwhite" :bg "gray"}

  ; Syntax items (:h group-name)
  Comment {:fg "lightgray"}

  Constant {}
  String {:fg "green"}
  Character {:fg "red"}
  Number {:link :Constant}
  Boolean {:link :Constant}
  Float {:link :Constant}

  Identifier {}
  Function {:fg "blue"}

  Statement {:fg "red"}
  Operator {}
  Repeat {:link :Statement}
  Conditional {:link :Statement}
  Label {:link :Statement}
  Keyword {:link :Statement}
  Exception {:link :Statement}

  PreProc {:fg "magenta"}
  Include {:fg "blue"}
  Define {:fg "magenta"}
  Macro {:fg "magenta"}
  PreCondit {:link :PreProc}

  Type {:fg "yellow"}
  StorageClass {:link :Type}
  Structure {:fg "yellow"}
  Typedef {:link :Type}

  Special {:fg "cyan"}
  SpecialChar {:link :Special}
  Tag {:fg "red"}
  Delimiter {}
  SpecialComment {:link :Special}
  Debug {:fg "red"}

  Underlined {:fg "normal" :attr "underline"}
  Bold {:attr "bold"}

  Ignore {:fg "black"}

  Error {:fg "red"}

  Todo {:fg "yellow" :bg "darkgray"}

  DiagnosticError {:fg "red"}
  DiagnosticWarn {:fg "yellow"}
  DiagnosticInfo {:fg "blue"}
  DiagnosticHint {:fg "lightgray"}
  DiagnosticSignError {:fg "red" :bg "darkgray"}
  DiagnosticSignWarn {:fg "yellow" :bg "darkgray"}
  DiagnosticSignInfo {:fg "blue" :bg "darkgray"}
  DiagnosticSignHint {:fg "lightgray" :bg "darkgray"}
  DiagnosticUnderlineError {:attr "undercurl" :guisp "red"}
  DiagnosticUnderlineWarn {:attr "undercurl" :guisp "yellow"}
  DiagnosticUnderlineInfo {:attr "undercurl" :guisp "blue"}
  DiagnosticUnderlineHint {:attr "undercurl" :guisp "lightgray"}

  ; Syntax-file specific highlighting
  diffAdded {:fg "green"}
  diffRemoved {:fg "red"}
  diffLine {:fg "cyan"}
  diffFile {:attr "bold"}
  diffIndexLine {:attr "bold"}
  diffSubname {:fg "normal"}

  gitcommitHeader {:fg "normal"}
  gitcommitSummary {:attr "bold"}
  gitcommitSelectedType {:fg "green"}
  gitcommitSelectedFile {:fg "green"}
  gitcommitDiscardedType {:fg "red"}
  gitcommitDiscardedFile {:fg "red"}
  gitcommitUntrackedFile {:fg "red"}
  gitcommitBranch {:fg "yellow"}

  gitrebaseHash {:fg "yellow"}
  gitrebaseSummary {:fg "normal"}
  gitrebasePick {:fg "green"}
  gitrebaseReword {:fg "blue"}
  gitrebaseEdit {:fg "red"}
  gitrebaseSquash {:fg "cyan"}
  gitrebaseFixup {:fg "cyan"}
  gitrebaseExec {:fg "cyan"}
  gitrebaseReset {:fg "cyan"}

  manSubHeading {:fg "red" :attr "bold"}
  manOptionDesc {:fg "red" :attr "bold"}
  manReference {:fg "yellow"}
  manUnderline {:fg "green" :attr "bold"}

  helpHeader {:fg "yellow"}
  helpSectionDelim {:fg "lightgray"}
  helpOption {:fg "yellow"}
  helpHyperTextJump {:fg "red"}

  luaTable {}

  vimOption {:fg "normal"}
  vimEnvvar {}
  vimVar {}
  vimFuncvar {}
  vimSpecial {}

  zigLabel {:fg "cyan"}

  ; Plugin highlighting
  GitSignsAdd {:fg "green" :bg "darkgray"}
  GitSignsDelete {:fg "red" :bg "darkgray"}
  GitSignsChange {:fg "lightgray" :bg "darkgray"}

  packerHash {:fg "yellow"}

  TSNone {:fg "normal"}
  TSSymbol {:fg "yellow"}
  TSDefinition {:link "MatchWord"}
  TSDefinitionUsage {:link "MatchWord"}
  TSParameter {}
  TSPunctBrakcet {}
  TSPunctDelimiter {}
  TSField {}
  TSProperty {}
  TSNamespace {}
  TSConstructor {}
  TSFuncBuiltin {:link "Function"}
  TSFuncMacro {:link "Function"}
  TSConstBuiltin {:link "Constant"}

  LspReferenceText {:link "MatchWord"}
  LspReferenceRead {:link "MatchWord"}
  LspReferenceWrite {:link "MatchWord"}

  debugPC {:fg "cyan" :bg "darkgray"}
  debugBreakpoint {:fg "red" :bg "darkgray"}

  Sneak {:link "IncSearch"}

  TelescopeMatching {:fg "yellow"}

  TreesitterContext {:bg "darkgray"})
