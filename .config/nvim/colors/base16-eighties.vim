" Base16 Eighties (https://github.com/chriskempson/base16)
" Scheme: Chris Kempson (http://chriskempson.com)
" Modified by: Gregory Anders

" GUI color definitions
let s:gui00 = '2d2d2d'
let s:gui01 = '393939'
let s:gui02 = '515151'
let s:gui03 = '747369'
let s:gui04 = 'a09f93'
let s:gui05 = 'd3d0c8'
let s:gui06 = 'e8e6df'
let s:gui07 = 'f2f0ec'
let s:gui08 = 'f2777a'
let s:gui09 = 'f99157'
let s:gui0A = 'ffcc66'
let s:gui0B = '99cc99'
let s:gui0C = '66cccc'
let s:gui0D = '6699cc'
let s:gui0E = 'cc99cc'
let s:gui0F = 'd27b53'

" Terminal color definitions
let s:cterm00 = '00'
let s:cterm03 = '08'
let s:cterm05 = '07'
let s:cterm07 = '15'
let s:cterm08 = '01'
let s:cterm0A = '03'
let s:cterm0B = '02'
let s:cterm0C = '06'
let s:cterm0D = '04'
let s:cterm0E = '05'
let s:cterm01 = '10'
let s:cterm02 = '11'
let s:cterm04 = '12'
let s:cterm06 = '13'
let s:cterm09 = '09'
let s:cterm0F = '14'

" Theme setup
hi clear
if exists('syntax_on')
  syntax reset
endif
let g:colors_name = 'base16-eighties'

" Highlighting function
fun s:hi(group, guifg, guibg, ctermfg, ctermbg, attr, guisp)
  if a:guifg !=# ''
    exec 'hi ' . a:group . ' guifg=#' . a:guifg
  endif
  if a:guibg !=# ''
    exec 'hi ' . a:group . ' guibg=#' . a:guibg
  endif
  if a:ctermfg !=# ''
    exec 'hi ' . a:group . ' ctermfg=' . a:ctermfg
  endif
  if a:ctermbg !=# ''
    exec 'hi ' . a:group . ' ctermbg=' . a:ctermbg
  endif
  if a:attr !=# ''
    exec 'hi ' . a:group . ' gui=' . a:attr . ' cterm=' . a:attr
  endif
  if a:guisp !=# ''
    exec 'hi ' . a:group . ' guisp=#' . a:guisp
  endif
endfun

" Vim editor colors
call s:hi('Bold',          '', '', '', '', 'bold', '')
call s:hi('Directory',     s:gui0D, '', s:cterm0D, '', '', '')
call s:hi('ErrorMsg',      s:gui08, s:gui00, s:cterm08, s:cterm00, '', '')
call s:hi('FoldColumn',    s:gui0C, s:gui01, s:cterm0C, s:cterm01, '', '')
call s:hi('Folded',        s:gui03, s:gui01, s:cterm03, s:cterm01, '', '')
call s:hi('IncSearch',     s:gui01, s:gui09, s:cterm01, s:cterm09, 'none', '')
call s:hi('Italic',        '', '', '', '', 'italic', '')
call s:hi('MatchParen',    '', s:gui03, '', s:cterm03,  '', '')
call s:hi('ModeMsg',       s:gui0B, '', s:cterm0B, '', '', '')
call s:hi('MoreMsg',       s:gui0B, '', s:cterm0B, '', '', '')
call s:hi('Question',      s:gui0D, '', s:cterm0D, '', '', '')
call s:hi('Search',        s:gui01, s:gui0A, s:cterm01, s:cterm0A,  '', '')
call s:hi('SpecialKey',    s:gui03, '', s:cterm03, '', '', '')
call s:hi('TooLong',       s:gui08, '', s:cterm08, '', '', '')
call s:hi('Visual',        '', s:gui02, '', s:cterm02, '', '')
call s:hi('VisualNOS',     s:gui08, '', s:cterm08, '', '', '')
call s:hi('WarningMsg',    s:gui0A, '', s:cterm0A, '', '', '')
call s:hi('WildMenu',      s:gui07, s:gui02, s:cterm07, s:cterm02, '', '')
call s:hi('Title',         s:gui0D, '', s:cterm0D, '', 'none', '')
call s:hi('Conceal',       s:gui0D, s:gui00, s:cterm0D, s:cterm00, '', '')
call s:hi('Cursor',        s:gui00, s:gui05, s:cterm00, s:cterm05, '', '')
call s:hi('NonText',       s:gui03, '', s:cterm03, '', '', '')
call s:hi('Normal',        s:gui05, s:gui00, s:cterm05, s:cterm00, '', '')
call s:hi('LineNr',        s:gui03, s:gui01, s:cterm03, s:cterm01, '', '')
call s:hi('SignColumn',    s:gui03, s:gui01, s:cterm03, s:cterm01, '', '')
call s:hi('StatusLine',    s:gui04, s:gui02, s:cterm04, s:cterm02, 'none', '')
call s:hi('StatusLineNC',  s:gui03, s:gui01, s:cterm03, s:cterm01, 'none', '')
call s:hi('VertSplit',     s:gui02, s:gui02, s:cterm02, s:cterm02, 'none', '')
call s:hi('ColorColumn',   '', s:gui01, '', s:cterm01, 'none', '')
call s:hi('CursorColumn',  '', s:gui01, '', s:cterm01, 'none', '')
call s:hi('CursorLine',    '', s:gui01, '', s:cterm01, 'none', '')
call s:hi('CursorLineNr',  s:gui04, s:gui01, s:cterm04, s:cterm01, '', '')
call s:hi('QuickFixLine',  '', s:gui01, '', s:cterm01, 'none', '')
call s:hi('PMenu',         s:gui05, s:gui01, s:cterm05, s:cterm01, 'none', '')
call s:hi('PMenuSel',      s:gui01, s:gui05, s:cterm01, s:cterm05, '', '')
call s:hi('TabLine',       s:gui03, s:gui01, s:cterm03, s:cterm01, 'none', '')
call s:hi('TabLineFill',   s:gui03, s:gui01, s:cterm03, s:cterm01, 'none', '')
call s:hi('TabLineSel',    s:gui0B, s:gui01, s:cterm0B, s:cterm01, 'none', '')

" Standard syntax highlighting
call s:hi('Boolean',      s:gui09, '', s:cterm09, '', '', '')
call s:hi('Character',    s:gui08, '', s:cterm08, '', '', '')
call s:hi('Comment',      s:gui03, '', s:cterm03, '', 'italic', '')
call s:hi('Conditional',  s:gui0E, '', s:cterm0E, '', '', '')
call s:hi('Constant',     s:gui09, '', s:cterm09, '', '', '')
call s:hi('Debug',         s:gui08, '', s:cterm08, '', '', '')
call s:hi('Define',       s:gui0E, '', s:cterm0E, '', 'none', '')
call s:hi('Delimiter',    s:gui0F, '', s:cterm0F, '', '', '')
call s:hi('Error',         s:gui00, s:gui08, s:cterm00, s:cterm08, '', '')
call s:hi('Exception',     s:gui08, '', s:cterm08, '', '', '')
call s:hi('Float',        s:gui09, '', s:cterm09, '', '', '')
call s:hi('Function',     s:gui0D, '', s:cterm0D, '', '', '')
call s:hi('Identifier',   s:gui08, '', s:cterm08, '', 'none', '')
call s:hi('Include',      s:gui0D, '', s:cterm0D, '', '', '')
call s:hi('Keyword',      s:gui0E, '', s:cterm0E, '', '', '')
call s:hi('Label',        s:gui0A, '', s:cterm0A, '', '', '')
call s:hi('Macro',         s:gui08, '', s:cterm08, '', '', '')
call s:hi('Number',       s:gui09, '', s:cterm09, '', '', '')
call s:hi('Operator',     s:gui05, '', s:cterm05, '', 'none', '')
call s:hi('PreProc',      s:gui0A, '', s:cterm0A, '', '', '')
call s:hi('Repeat',       s:gui0E, '', s:cterm0E, '', '', '')
call s:hi('Special',      s:gui0C, '', s:cterm0C, '', '', '')
call s:hi('SpecialChar',  s:gui0F, '', s:cterm0F, '', '', '')
call s:hi('Statement',    s:gui08, '', s:cterm08, '', '', '')
call s:hi('StorageClass', s:gui0A, '', s:cterm0A, '', '', '')
call s:hi('String',       s:gui0B, '', s:cterm0B, '', '', '')
call s:hi('Structure',    s:gui0E, '', s:cterm0E, '', '', '')
call s:hi('Tag',          s:gui0A, '', s:cterm0A, '', '', '')
call s:hi('Todo',         s:gui0A, s:gui01, s:cterm0A, s:cterm01, '', '')
call s:hi('Type',         s:gui0A, '', s:cterm0A, '', 'none', '')
call s:hi('Typedef',      s:gui0A, '', s:cterm0A, '', '', '')
call s:hi('Underlined',    'none', '', 'none', '', '', '')

" Diff highlighting
call s:hi('DiffAdd',      s:gui0B, s:gui01,  s:cterm0B, s:cterm01, '', '')
call s:hi('DiffChange',   s:gui03, s:gui01,  s:cterm03, s:cterm01, '', '')
call s:hi('DiffDelete',   s:gui08, s:gui01,  s:cterm08, s:cterm01, '', '')
call s:hi('DiffText',     s:gui0D, s:gui01,  s:cterm0D, s:cterm01, '', '')
call s:hi('DiffAdded',    s:gui0B, s:gui00,  s:cterm0B, s:cterm00, '', '')
call s:hi('DiffFile',     s:gui08, s:gui00,  s:cterm08, s:cterm00, '', '')
call s:hi('DiffNewFile',  s:gui0B, s:gui00,  s:cterm0B, s:cterm00, '', '')
call s:hi('DiffLine',     s:gui0D, s:gui00,  s:cterm0D, s:cterm00, '', '')
call s:hi('DiffRemoved',  s:gui08, s:gui00,  s:cterm08, s:cterm00, '', '')

" Spelling highlighting
call s:hi('SpellBad',     '', '', s:cterm08, 'none', 'undercurl', s:gui08)
call s:hi('SpellLocal',   '', '', s:cterm0D, 'none', 'undercurl', s:gui0C)
call s:hi('SpellCap',     '', '', s:cterm0E, 'none', 'undercurl', s:gui0D)
call s:hi('SpellRare',    '', '', s:cterm0C, 'none', 'undercurl', s:gui0E)
