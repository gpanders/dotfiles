" Base16 Eighties (https://github.com/chriskempson/base16)
" Scheme: Chris Kempson (http://chriskempson.com)
" Modified by: Gregory Anders

if exists('g:colors_name')
    hi clear
endif

let g:colors_name = 'base16-eighties'

let s:gui_black = '2d2d2d'
let s:gui_red = 'f2777a'
let s:gui_green = '99cc99'
let s:gui_yellow = 'ffcc66'
let s:gui_blue = '6699cc'
let s:gui_magenta = 'cc99cc'
let s:gui_cyan = '66cccc'
let s:gui_white = 'd3d0c8'
let s:gui_brblack = '747369'
let s:gui_brred = 'f99157'
let s:gui_bryellow = '515151'
let s:gui_brgreen = '393939'
let s:gui_brblue = 'a09f93'
let s:gui_brmagenta = 'e8e6df'
let s:gui_brcyan = 'd27b53'
let s:gui_brwhite = 'f2f0ec'

let s:cterm_black = '00'
let s:cterm_red = '01'
let s:cterm_green = '02'
let s:cterm_yellow = '03'
let s:cterm_blue = '04'
let s:cterm_magenta = '05'
let s:cterm_cyan = '06'
let s:cterm_white = '07'
let s:cterm_brblack = '08'
let s:cterm_brred = '09'
let s:cterm_brgreen = '10'
let s:cterm_bryellow = '11'
let s:cterm_brblue = '12'
let s:cterm_brmagenta = '13'
let s:cterm_brcyan = '14'
let s:cterm_brwhite = '15'

function! s:hi(group, fg, bg, attr, guisp)
    exec 'hi clear ' . a:group
    let s = 'hi ' . a:group
    if a:fg !=# ''
        let s .= ' ctermfg=' . s:cterm_{a:fg} . ' guifg=#' . s:gui_{a:fg}
    endif
    if a:bg !=# ''
        let s .= ' ctermbg=' . s:cterm_{a:bg} . ' guibg=#' . s:gui_{a:bg}
    endif
    if a:attr !=# ''
        let s .= ' gui=' . a:attr . ' cterm=' . a:attr
    endif
    if a:guisp !=# ''
        let s .= ' guisp=#' . a:guisp
    endif
    exec s
endfunction

" UI Highlights (:h highlight-groups)
call s:hi('ColorColumn',  '', 'brgreen', '', '')
call s:hi('Conceal',      'blue', 'black', '', '')
call s:hi('Cursor',       'black', 'white', '', '')
call s:hi('CursorColumn', '', 'brgreen', 'none', '')
call s:hi('CursorLine',   '', 'brgreen', 'none', '')
call s:hi('Directory',    'blue', '', '', '')
call s:hi('DiffAdd',      'green', '',  '', '')
call s:hi('DiffChange',   'brblack', '',  '', '')
call s:hi('DiffDelete',   'red', '',  '', '')
call s:hi('DiffText',     'blue', '',  '', '')
call s:hi('ErrorMsg',     'red', 'black', '', '')
call s:hi('VertSplit',    'bryellow', 'bryellow', 'none', '')
call s:hi('Folded',       'brblack', 'brgreen', '', '')
call s:hi('FoldColumn',   'cyan', 'brgreen', '', '')
call s:hi('SignColumn',   'brblack', 'brgreen', '', '')
call s:hi('IncSearch',    'brgreen', 'brred', 'none', '')
call s:hi('LineNr',       'brblack', 'brgreen', '', '')
call s:hi('CursorLineNr', 'brblue', 'brgreen', '', '')
call s:hi('MatchParen',   '', 'brblack', '', '')
call s:hi('ModeMsg',      'green', '', '', '')
call s:hi('MoreMsg',      'green', '', '', '')
call s:hi('NonText',      'brblack', '', '', '')
call s:hi('Normal',       'white', 'black', 'none', '')
call s:hi('PMenu',        'white', 'brgreen', 'none', '')
call s:hi('PMenuSel',     'brgreen', 'white', '', '')
call s:hi('Question',     'blue', '', '', '')
call s:hi('QuickFixLine', '', 'brgreen', 'none', '')
call s:hi('Search',       'brgreen', 'yellow', '', '')
call s:hi('SpecialKey',   'brblack', '', '', '')
call s:hi('SpellBad',     'red', '', 'undercurl', 'red')
call s:hi('SpellLocal',   'blue', '', 'undercurl', 'cyan')
call s:hi('SpellCap',     'magenta', '', 'undercurl', 'blue')
call s:hi('SpellRare',    'cyan', '', 'undercurl', 'magenta')
call s:hi('StatusLine',   'brblue', 'bryellow', 'none', '')
call s:hi('StatusLineNC', 'brblack', 'brgreen', 'none', '')
call s:hi('TabLine',      'brblack', 'brgreen', 'none', '')
call s:hi('TabLineFill',  'brblack', 'brgreen', 'none', '')
call s:hi('TabLineSel',   'green', 'brgreen', 'none', '')
call s:hi('Title',        'blue', '', 'none', '')
call s:hi('Visual',       '', 'bryellow', '', '')
call s:hi('VisualNOS',    'red', '', '', '')
call s:hi('WarningMsg',   'yellow', '', '', '')
call s:hi('WildMenu',     'brwhite', 'bryellow', '', '')

" Syntax items (:h group-name)
call s:hi('Comment', 'brblack', '', 'italic', '')

call s:hi('Constant',  'brred', '', '', '')
call s:hi('String',    'green', '', '', '')
call s:hi('Character', 'red', '', '', '')
call s:hi('Number',    'brred', '', '', '')
call s:hi('Boolean',   'brred', '', '', '')
call s:hi('Float',     'brred', '', '', '')

call s:hi('Identifier', 'red', '', '', '')
call s:hi('Function',   'blue', '', '', '')

call s:hi('Statement',   'red', '', '', '')
call s:hi('Operator',    'white', '', 'none', '')
call s:hi('Repeat',      'magenta', '', '', '')
call s:hi('Conditional', 'magenta', '', '', '')
call s:hi('Label',       'yellow', '', '', '')
call s:hi('Keyword',     'magenta', '', '', '')
call s:hi('Exception',   'red', '', '', '')

call s:hi('PreProc',   'yellow', '', '', '')
call s:hi('Include',   'blue', '', '', '')
call s:hi('Define',    'magenta', '', '', '')
call s:hi('Macro',     'red', '', '', '')
call s:hi('PreCondit', 'yellow', '', '', '')

call s:hi('Type',         'yellow', '', 'none', '')
call s:hi('StorageClass', 'yellow', '', 'none', '')
call s:hi('Structure',    'magenta', '', '', '')
call s:hi('Typedef',      'yellow', '', 'none', '')

call s:hi('Special',        'cyan', '', '', '')
call s:hi('SpecialChar',    'brcyan', '', '', '')
call s:hi('Tag',            'yellow', '', '', '')
call s:hi('Delimiter',      'brcyan', '', '', '')
call s:hi('SpecialComment', 'cyan', '', '', '')
call s:hi('Debug',          'red', '', '', '')

call s:hi('Underlined', '', '', 'underline', '')

call s:hi('Ignore', 'black', 'black', 'none', '')

call s:hi('Error', 'black', 'red', '', '')

call s:hi('Todo', 'yellow', 'brgreen', '', '')

" Syntax-file specific highlighting
call s:hi('diffAdded',     'green', '', '', '')
call s:hi('diffRemoved',   'red', '', '', '')
call s:hi('diffLine',      'cyan', '', '', '')
call s:hi('diffFile',      '', '', 'bold', '')
call s:hi('diffIndexLine', '', '', 'bold', '')

call s:hi('gitcommitHeader',        'white', '', '', '')
call s:hi('gitcommitSelectedType',  'green', '', '', '')
call s:hi('gitcommitSelectedFile',  'green', '', '', '')
call s:hi('gitcommitDiscardedType', 'red', '', '', '')
call s:hi('gitcommitDiscardedFile', 'red', '', '', '')
call s:hi('gitcommitBranch',        'yellow', '', '', '')

call s:hi('gitrebasePick',   'yellow', '', '', '')
call s:hi('gitrebaseReword', 'blue', '', '', '')
call s:hi('gitrebaseEdit',   'red', '', '', '')
call s:hi('gitrebaseSquash', 'magenta', '', '', '')
call s:hi('gitrebaseFixup',  'magenta', '', '', '')
call s:hi('gitrebaseExec',   'cyan', '', '', '')
call s:hi('gitrebaseReset',  'magenta', '', '', '')

" Plugin highlighting
call s:hi('GitSignsAdd',    'green', 'brgreen', '', '')
call s:hi('GitSignsDelete', 'red', 'brgreen', '', '')
call s:hi('GitSignsChange', 'brblack', 'brgreen', '', '')
