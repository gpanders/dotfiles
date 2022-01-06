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
             :white "#d3d0c8"
             :brblack "#747369"
             :brred "#f99157"
             :bryellow "#515151"
             :brgreen "#393939"
             :brblue "#a09f93"
             :brmagenta "#e8e6df"
             :brcyan "#d27b53"
             :brwhite "#f2f0ec"}
        cterm {:black 0
               :red 1
               :green 2
               :yellow 3
               :blue 4
               :magenta 5
               :cyan 6
               :white 7
               :brblack 8
               :brred 9
               :brgreen 10
               :bryellow 11
               :brblue 12
               :brmagenta 13
               :brcyan 14
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
  Normal {:fg "white" :bg "black"}

  ; UI Highlights (:h highlight-groups)
  ColorColumn {:bg "brgreen"}
  Conceal {:fg "blue"}
  Cursor {:fg "black" :bg "white"}
  CursorColumn {:bg "brgreen"}
  CursorLine {:bg "brgreen"}
  Directory {:fg "blue"}
  DiffAdd {:fg "green"}
  DiffChange {:fg "brblack"}
  DiffDelete {:fg "red"}
  DiffText {:fg "blue"}
  ErrorMsg {:fg "red"}
  VertSplit {:fg "bryellow" :bg "bryellow"}
  Folded {:fg "brblack" :bg "brgreen"}
  FoldColumn {:fg "cyan" :bg "brgreen"}
  SignColumn {:fg "brblack" :bg "brgreen"}
  IncSearch {:fg "brgreen" :bg "brred"}
  LineNr {:fg "brblack" :bg "brgreen"}
  CursorLineNr {:fg "brblue" :bg "brgreen"}
  MatchParen {:bg "brblack"}
  MatchWord {:bg "brgreen"}
  ModeMsg {:fg "green"}
  MoreMsg {:fg "green"}
  NonText {:fg "brblack"}
  PMenu {:bg "brgreen"}
  PMenuSel {:fg "brgreen" :bg "white"}
  Question {:fg "blue"}
  QuickFixLine {:bg "brgreen"}
  Search {:fg "brgreen" :bg "yellow"}
  SpecialKey {:fg "brblack"}
  SpellBad {:fg "red" :attr "undercurl" :guisp "red"}
  SpellLocal {:fg "blue" :attr "undercurl" :guisp "cyan"}
  SpellCap {:fg "magenta" :attr "undercurl" :guisp "blue"}
  SpellRare {:fg "cyan" :attr "undercurl" :guisp "magenta"}
  StatusLine {:fg "brblue" :bg "bryellow"}
  StatusLineNC {:fg "brblack" :bg "brgreen"}
  TabLine {:fg "brblack" :bg "brgreen"}
  TabLineFill {:fg "brblack" :bg "brgreen"}
  TabLineSel {:fg "green" :bg "brgreen"}
  Title {:fg "blue"}
  Visual {:bg "bryellow"}
  VisualNOS {:fg "red"}
  WarningMsg {:fg "yellow"}
  WildMenu {:fg "brwhite" :bg "bryellow"}

  ; Syntax items (:h group-name)
  Comment {:fg "brblack"}

  Constant {:fg "brred"}
  String {:fg "green"}
  Character {:fg "red"}
  Number {:fg "white"}
  Boolean {:fg "white"}
  Float {:fg "white"}

  Identifier {:fg "red"}
  Function {:fg "blue"}

  Statement {:fg "red"}
  Operator {:fg "white"}
  Repeat {:fg "red"}
  Conditional {:fg "red"}
  Label {:fg "white"}
  Keyword {:fg "red"}
  Exception {:fg "red"}

  PreProc {:fg "yellow"}
  Include {:fg "blue"}
  Define {:fg "magenta"}
  Macro {:fg "magenta"}
  PreCondit {:fg "magenta"}

  Type {:fg "yellow"}
  StorageClass {:fg "yellow"}
  Structure {:fg "NONE"}
  Typedef {:fg "yellow"}

  Special {:fg "cyan"}
  SpecialChar {:fg "cyan"}
  Tag {:fg "red"}
  Delimiter {:fg "brcyan"}
  SpecialComment {:fg "cyan"}
  Debug {:fg "cyan"}

  Underlined {:attr "underline"}
  Bold {:attr "bold"}

  Ignore {:fg "black"}

  Error {:fg "red"}

  Todo {:fg "yellow" :bg "brgreen"}

  DiagnosticError {:fg "red"}
  DiagnosticWarn {:fg "yellow"}
  DiagnosticInfo {:fg "blue"}
  DiagnosticHint {:fg "brblack"}
  DiagnosticSignError {:fg "red" :bg "brgreen"}
  DiagnosticSignWarn {:fg "yellow" :bg "brgreen"}
  DiagnosticSignInfo {:fg "blue" :bg "brgreen"}
  DiagnosticSignHint {:fg "brblack" :bg "brgreen"}
  DiagnosticUnderlineError {:fg "red" :attr "undercurl" :guisp "red"}
  DiagnosticUnderlineWarn {:fg "yellow" :attr "undercurl" :guisp "yellow"}
  DiagnosticUnderlineInfo {:fg "blue" :attr "undercurl" :guisp "blue"}
  DiagnosticUnderlineHint {:fg "brblack" :attr "undercurl" :guisp "brblack"}

  ; Syntax-file specific highlighting
  diffAdded {:fg "green"}
  diffRemoved {:fg "red"}
  diffLine {:fg "cyan"}
  diffFile {:attr "bold"}
  diffIndexLine {:attr "bold"}
  diffSubname {:fg "white"}

  gitcommitHeader {:fg "white"}
  gitcommitSummary {:attr "bold"}
  gitcommitSelectedType {:fg "green"}
  gitcommitSelectedFile {:fg "green"}
  gitcommitDiscardedType {:fg "red"}
  gitcommitDiscardedFile {:fg "red"}
  gitcommitUntrackedFile {:fg "red"}
  gitcommitBranch {:fg "yellow"}

  gitrebaseHash {:fg "yellow"}
  gitrebaseSummary {:fg "white"}
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

  ; Plugin highlighting
  GitSignsAdd {:fg "green" :bg "brgreen"}
  GitSignsDelete {:fg "red" :bg "brgreen"}
  GitSignsChange {:fg "brblack" :bg "brgreen"}

  packerHash {:fg "yellow"}

  TSNone {:fg "white"}
  TSSymbol {:fg "yellow"}
  TSDefinition {:link "MatchWord"}
  TSDefinitionUsage {:link "MatchWord"}

  LspReferenceText {:link "MatchWord"}
  LspReferenceRead {:link "MatchWord"}
  LspReferenceWrite {:link "MatchWord"}

  DapBreakpoint {:fg "cyan" :bg "brgreen"}
  DapBreakpointCondition {:fg "cyan" :bg "brgreen"}
  DapLogPoint {:fg "cyan" :bg "brgreen"}
  DapStopped {:fg "brwhite" :bg "brgreen"}
  DapBreakpointRejected {:fg "red" :bg "brgreen"}

  Sneak {:link "IncSearch"})
