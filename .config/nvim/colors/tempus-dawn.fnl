; Name: Tempus Dawn
; Description: Light theme with a soft, slightly desaturated palette (WCAG AA compliant)
; Author: Protesilaos Stavrou (https://protesilaos.com)
; Meta: Created with the Tempus Themes Generator
; URL: https://gitlab.com/protesilaos/tempus-themes-generator
; Modified by: Gregory Anders

(when vim.g.colors_name
  (exec "hi clear"))

(set vim.g.colors_name :anders)
(set vim.o.background :light)

; Terminal
; --------

(set vim.g.terminal_color_0 :black)
(set vim.g.terminal_color_1 :red)
(set vim.g.terminal_color_2 :green)
(set vim.g.terminal_color_3 :yellow)
(set vim.g.terminal_color_4 :blue)
(set vim.g.terminal_color_5 :magenta)
(set vim.g.terminal_color_6 :cyan)
(set vim.g.terminal_color_7 :white)
(set vim.g.terminal_color_8 :brblack)
(set vim.g.terminal_color_9 :brred)
(set vim.g.terminal_color_10 :brgreen)
(set vim.g.terminal_color_11 :bryellow)
(set vim.g.terminal_color_12 :brblue)
(set vim.g.terminal_color_13 :brmagenta)
(set vim.g.terminal_color_14 :brcyan)
(set vim.g.terminal_color_15 :brwhite)

(macro make-colors [...]
  (assert-compile (= 0 (math.fmod (select :# ...) 2))
                  "expected even number of group/option pairs")
  (let [colors {:black     {:gui "#4a4b4e" :cterm 0}
                :red       {:gui "#a32a3a" :cterm 1}
                :green     {:gui "#206620" :cterm 2}
                :yellow    {:gui "#745300" :cterm 3}
                :blue      {:gui "#4b529a" :cterm 4}
                :magenta   {:gui "#8d377e" :cterm 5}
                :cyan      {:gui "#086784" :cterm 6}
                :white     {:gui "#dee2e0" :cterm 7}
                :brblack   {:gui "#676364" :cterm 8}
                :brred     {:gui "#a64822" :cterm 9}
                :brgreen   {:gui "#187408" :cterm 10}
                :bryellow  {:gui "#8b590a" :cterm 11}
                :brblue    {:gui "#5c59b2" :cterm 12}
                :brmagenta {:gui "#8e45a8" :cterm 13}
                :brcyan    {:gui "#3f649c" :cterm 14}
                :brwhite   {:gui "#eff0f2" :cterm 15}}
        highlights []]
    (for [i 1 (select :# ...) 2]
      (let [(group opts) (select i ...)
            group (tostring group)
            s (if opts.link
                  (: "hi link %s %s" :format group opts.link)
                  (let [fg (. colors opts.fg)
                        bg (. colors opts.bg)]
                    (: "hi %s guifg=%s guibg=%s ctermfg=%s ctermbg=%s gui=%s cterm=%s" :format
                       group
                       (or (?. fg :gui) :NONE)
                       (or (?. bg :gui) :NONE)
                       (or (?. fg :cterm) :NONE)
                       (or (?. bg :cterm) :NONE)
                       (or opts.attr :NONE)
                       (or opts.attr :NONE))))]
        (table.insert highlights s)))
    `(vim.cmd ,(table.concat highlights "\n"))))

(make-colors
  Normal {:bg "brwhite" :fg "black"}
  Visual {:bg "black" :fg "brwhite"}
  Search {:attr "underline,bold" :bg "white" :fg "black"}
  IncSearch {:attr "underline,bold" :bg "brblack" :fg "brwhite"}

  StatusLine {:bg "black" :fg "brwhite"}
  StatusLineNC {:bg "white" :fg "brblack"}
  StatusLineTerm {:bg "green" :fg "brwhite"}
  StatusLineTermNC {:bg "white" :fg "green"}

  VertSplit {}
  TabLine {:bg "white" :fg "brblack"}
  TabLineSel {:bg "cyan" :fg "brwhite"}
  TabLineFill {}

  Comment {:attr "italic" :fg "brblack"}
  Todo {:attr "bold" :bg "white" :fg "bryellow"}

  Warning {:bg "yellow" :fg "brwhite"}
  WarningMsg {:bg "yellow" :fg "brwhite"}
  Error {:bg "red" :fg "brwhite"}
  ErrorMsg {:bg "red" :fg "brwhite"}

  MatchParen {:attr "underline,bold" :bg "white" :fg "brblack"}

  ToolbarLine {:bg "brblack" :fg "brwhite"}
  ToolbarButton {:attr "bold" :bg "brblack" :fg "brwhite"}

  WildMenu {:bg "brwhite" :fg "black"}

  Terminal {:bg "brwhite" :fg "black"}

  ; Constructs
  ; ----------
  Constant {:fg "blue"}
  Number {:fg "blue"}
  Float {:fg "blue"}
  String {:fg "brblue"}

  Function {:fg "magenta"}
  Identifier {:fg "brmagenta"}
  Label {:fg "magenta"}
  Tag {:fg "magenta"}
  Keyword {:attr "bold" :fg "brmagenta" :attr "bold"}

  Character {:attr "bold" :fg "brcyan"}

  Type {:attr "none,bold" :fg "cyan"}
  Boolean {:fg "cyan"}
  StorageClass {:fg "cyan"}
  Structure {:fg "cyan"}
  Typedef {:attr "bold" :fg "brcyan"}

  Conditional {:attr "bold" :fg "green"}
  Statement {:fg "brgreen"}
  Repeat {:attr "bold" :fg "brgreen"}
  Operator {:attr "bold" :fg "black"}
  Exception {:attr "bold" :fg "red"}

  Preproc {:fg "brred"}
  PreCondit {:attr "bold" :fg "brred"}
  Macro {:attr "bold" :fg "brred"}
  Include {:fg "brred"}
  Define {:fg "brred"}

  Title {:attr "bold" :bg "brwhite" :fg "cyan"}

  Delimiter {}
  SpecialComment {:attr "bold" :fg "magenta"}

  Debug {:fg "brmagenta"}

  ; Other
  ; -----
  LineNr {:bg "white" :fg "brblack"}
  Cursor {:bg "black" :fg "brwhite"}
  CursorLine {:bg "white"}
  CursorColumn {:bg "white"}
  CursorLineNr {:attr "bold" :bg "brblack" :fg "brwhite"}
  ColorColumn {:bg "white" :fg "black"}
  SignColumn {:bg "white" :fg "brblack"}

  Folded {:bg "white" :fg "brblack"}
  FoldColumn {:bg "white" :fg "brblack"}

  Special {:attr "bold" :fg "bryellow"}
  SpecialKey {:bg "white" :fg "brblack"}
  SpecialChar {:attr "bold" :fg "bryellow"}
  NonText {:fg "brblack"}
  EndOfBuffer {:attr "bold" :fg "brblack"}

  Directory {:fg "green"}
  Question {:attr "bold" :fg "bryellow"}
  MoreMsg {:fg "brgreen"}
  ModeMsg {:attr "bold" :fg "green"}

  VimOption {:fg "magenta"}
  VimGroup {:fg "magenta"}

  Underlined {:attr "underline" :fg "black"}
  Ignore {:bg "white" :fg "brblack"}
  Conceal {:bg "brblack" :fg "white"}

  SpellBad {:bg "red" :fg "brwhite"}
  SpellCap {:bg "yellow" :fg "brwhite"}
  SpellRare {:bg "brmagenta" :fg "brwhite"}
  SpellLocal {:bg "brcyan" :fg "brwhite"}

  Pmenu {:attr "italic" :bg "white" :fg "black"}
  PmenuSel {:attr "none,bold" :bg "brblack" :fg "brwhite"}
  PmenuSbar {:bg "white"}
  PmenuThumb {:bg "brblack"}

  ; Diffs
  ; -----
  DiffAdd {:attr "bold" :bg "green" :fg "brwhite"}
  DiffDelete {:bg "red" :fg "brwhite"}
  DiffChange {:attr "bold" :bg "white" :fg "brblack"}
  DiffText {:attr "bold" :bg "white" :fg "brred"}

  diffAdded {:fg "green"}
  diffRemoved {:fg "red"}
  diffNewFile {:fg "blue"}
  diffFile {:fg "yellow"}

  ; Markdown
  ; --------
  MarkdownRule {:attr "bold" :bg "white" :fg "brgreen"}

  MarkdownHeading {:attr "bold" :fg "black"}
  MarkdownH1 {:link :MarkdownHeading}
  MarkdownH2 {:link :MarkdownHeading}
  MarkdownH3 {:link :MarkdownHeading}
  MarkdownH4 {:link :MarkdownHeading}
  MarkdownH5 {:link :MarkdownHeading}
  MarkdownH6 {:link :MarkdownHeading}
  MarkdownHeadingDelimiter {:link :MarkdownHeading}
  MarkdownHeadingRule {:link :MarkdownHeading}

  MarkdownBold {:attr "bold" :fg "brred"}
  MarkdownBoldDelimiter {:link :MarkdownBold}

  MarkdownItalic {:attr "italic" :fg "yellow"}
  MarkdownItalicDelimiter {:link :MarkdownItalic}

  MarkdownUrl {:attr "underline" :fg "blue"}
  MarkdownLinkText {:fg "brblue"}
  MarkdownLinkDelimiter {:attr "bold" :fg "black"}
  MarkdownLinkTextDelimiter {:link :MarkdownLinkDelimiter}

  MarkdownCode {:fg "magenta"}
  MarkdownCodeDelimiter {:link :MarkdownCode}

  MarkdownCodeBlock {:fg "black"}

  MarkdownListMarker {:fg "green"}
  MarkdownOrderedListMarker {:link :MarkdownListMarker}

  ; Linting
  ; -------
  DiagnosticError {:fg "red"}
  DiagnosticWarn {:fg "yellow"}
  DiagnosticInfo {:fg "blue"}
  DiagnosticHint {:fg "brblack"}
  DiagnosticSignError {:fg "red" :bg "white"}
  DiagnosticSignWarn {:fg "yellow" :bg "white"}
  DiagnosticSignInfo {:fg "blue" :bg "white"}
  DiagnosticSignHint {:fg "brblack" :bg "white"}
  DiagnosticUnderlineError {:fg "red" :attr "underline"}
  DiagnosticUnderlineWarn {:fg "yellow" :attr "underline"}
  DiagnosticUnderlineInfo {:fg "blue" :attr "underline"}
  DiagnosticUnderlineHint {:fg "brblack" :attr "underline"}

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

  vimOption {}
  vimVar {}
  vimEnvvar {}

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

  Sneak {:link "IncSearch"}

  TelescopeSelection {:bg "lightest_gray" :attr "bold"}
  TelescopeMatching {:fg "orange"})
