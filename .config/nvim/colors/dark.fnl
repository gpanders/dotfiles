(when vim.g.colors_name
  (exec "hi clear"))

(set vim.g.colors_name :base16-eighties)

(macro make-colors [...]
  (assert-compile (= 0 (math.fmod (select :# ...) 2))
                  "expected even number of group/option pairs")
  (let [gui {:black "#2d2d2d"
             :red "#f2777a"
             :green "#99cc99"
             :yellow "#ffcc66"
             :blue "#6699cc"
             :magenta "#cc99cc"
             :cyan "#66cccc"
             :normal "#d3d0c8"
             :lightgray "#747369"
             :orange "#f99157"
             :gray "#515151"
             :darkgray "#393939"
             :lightergray "#a09f93"
             :white "#e8e6df"
             :darkorange "#d27b53"
             :brwhite "#f2f0ec"}
        cterm {:black 0
               :red 1
               :green 2
               :yellow 3
               :blue 4
               :magenta 5
               :cyan 6
               :normal 7
               :lightgray 8
               :orange 9
               :darkgray 10
               :gray 11
               :lightergray 12
               :white 13
               :darkorange 14
               :brwhite 15}
        highlights []]
    (for [i 1 (select :# ...) 2]
      (let [(group opts) (select i ...)
            group (tostring group)
            s (if opts.link
                  (: "hi link %s %s" :format group opts.link)
                  (: "hi %s ctermfg=%s ctermbg=%s guifg=%s guibg=%s cterm=%s gui=%s guisp=%s" :format
                     group
                     (or (. cterm opts.fg) :NONE)
                     (or (. cterm opts.bg) :NONE)
                     (or (. gui opts.fg) :NONE)
                     (or (. gui opts.bg) :NONE)
                     (or opts.attr :NONE)
                     (or opts.attr :NONE)
                     (or (. gui opts.guisp) :NONE)))]
        (table.insert highlights s)))
    `(vim.cmd ,(table.concat highlights "\n"))))

(make-colors
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
  NormalFloat {:link :Normal}
  PMenu {:bg "darkgray"}
  PMenuSel {:fg "darkgray" :bg "normal"}
  Question {:fg "blue"}
  QuickFixLine {:bg "darkgray"}
  Search {:fg "darkgray" :bg "yellow"}
  SignColumn {:fg "lightgray" :bg "darkgray"}
  SpecialKey {:fg "green"}
  SpellBad {:fg "red" :attr "undercurl" :guisp "red"}
  SpellCap {:fg "magenta" :attr "undercurl" :guisp "blue"}
  SpellLocal {:fg "blue" :attr "undercurl" :guisp "cyan"}
  SpellRare {:fg "cyan" :attr "undercurl" :guisp "magenta"}
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

  Constant {:fg "normal"}
  String {:fg "green"}
  Character {:link :Constant}
  Number {:link :Constant}
  Boolean {:link :Constant}
  Float {:link :Constant}

  Identifier {:fg "red"}
  Function {:fg "blue"}

  Statement {:fg "red"}
  Operator {:fg "normal" :attr :bold}
  Repeat {:link :Statement}
  Conditional {:link :Statement}
  Label {:link :Statement}
  Keyword {:link :Statement}
  Exception {:link :Statement}

  PreProc {:fg "lightgray"}
  Include {:fg "blue"}
  Define {:link :PreProc}
  Macro {:fg "magenta"}
  PreCondit {:link :PreProc}

  Type {:fg "yellow"}
  StorageClass {:link :Type}
  Structure {:link :Type}
  Typedef {:link :Type}

  Special {:fg "cyan"}
  SpecialChar {:link :Special}
  Tag {:link :Special}
  Delimiter {}
  SpecialComment {:link :Special}
  Debug {:link :Special}

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
  DiagnosticUnderlineError {:fg "red" :attr "undercurl" :guisp "red"}
  DiagnosticUnderlineWarn {:fg "yellow" :attr "undercurl" :guisp "yellow"}
  DiagnosticUnderlineInfo {:fg "blue" :attr "undercurl" :guisp "blue"}
  DiagnosticUnderlineHint {:fg "lightgray" :attr "undercurl" :guisp "lightgray"}

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

  helpOption {:fg "yellow"}
  helpHyperTextJump {:fg "red"}

  luaTable {}

  vimOption {:fg "normal"}
  vimEnvvar {}
  vimVar {}
  vimFuncvar {}
  vimSpecial {}

  ; Plugin highlighting
  GitSignsAdd {:fg "green" :bg "darkgray"}
  GitSignsDelete {:fg "red" :bg "darkgray"}
  GitSignsChange {:fg "lightgray" :bg "darkgray"}

  packerHash {:fg "yellow"}

  TSNone {:fg "normal"}
  TSSymbol {:fg "yellow"}
  TSDefinition {:link "MatchWord"}
  TSDefinitionUsage {:link "MatchWord"}

  LspReferenceText {:link "MatchWord"}
  LspReferenceRead {:link "MatchWord"}
  LspReferenceWrite {:link "MatchWord"}

  debugPC {:fg "cyan" :bg "darkgray"}
  debugBreakpoint {:fg "red" :bg "darkgray"}

  Sneak {:link "IncSearch"}

  TelescopeMatching {:fg "yellow"})
