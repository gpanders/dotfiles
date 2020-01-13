" Set the statusline

" Initialize StatuslineMode highlight group
highlight! link StatuslineMode StatuslineModeNormal

function! StatusLine(sep)
  " Reset the statusline
  let s = ''

  " Set highlight to StatuslineMode
  let s .= '%#StatuslineMode# '

  " Show mode
  let m = mode()
  if m ==# 'n'
    highlight! link StatuslineMode StatuslineModeNormal
    let s .= 'N'
  elseif m ==# 'i'
    highlight! link StatuslineMode StatuslineModeInsert
    let s .= 'I'
  elseif m ==# 'R'
    highlight! link StatuslineMode StatuslineModeReplace
    let s .= 'R'
  elseif m ==? 'v' || m ==# ''
    highlight! link StatuslineMode StatuslineModeVisual
    let s .= 'V'
  else
    highlight! link StatuslineMode StatuslineModeNormal
    let s .= toupper(m)
  endif

  " Set highlight to User1
  let s .= ' %1* '

  " Filename (truncated if necessary)
  let s .= '%<%f'

  " File flags (modified and read-only)
  if &modified
    let s .= ' +'
  endif
  if &readonly
    let s .= ' [RO]'
  endif

  " Set highlight to User2
  let s .= ' %2* '

  " Right justify
  let s .= '%='

  " Set highlight to User3
  let s .= ' %3* '

  " Show git branch, if available
  if exists('*FugitiveHead')
    let branch = FugitiveHead()
    if branch !=# ''
      let s .= branch . ' ' . a:sep . ' '
    endif
  endif

  " Show line break style
  if &fileformat ==# 'unix'
    let s .= 'LF ' . a:sep . ' '
  elseif &fileformat ==# 'dos'
    let s .= 'CRLF ' . a:sep . ' '
  elseif &fileformat ==# 'mac'
    let s .= 'CR ' . a:sep . ' '
  endif

  " Show file type
  if &filetype ==# ''
    let s .= 'none'
  else
    let s .= &filetype
  endif

  " Set highlight to User4
  let s .= ' %4* '

  " Position in file
  let s .= '%(%l:%c%V%) %P '

  return s
endfunction

set laststatus=2
set noshowmode
set statusline=%!StatusLine('/')
