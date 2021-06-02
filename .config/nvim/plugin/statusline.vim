highlight! link StatuslineMode StatuslineModeNormal

function! FugitiveStatusLine(sep)
  if get(g:, 'loaded_fugitive')
    let branch = fugitive#head()
    if branch !=# ''
      return branch . ' ' . a:sep . ' '
    endif
  endif
  return ''
endfunction

function! StatusLine(sep)
  let s = ''

  " Set highlight to StatuslineMode
  let s .= '%#StatuslineMode# '

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
  let s .= '%{&modified ? " +" : ""}'
  let s .= '%{&readonly ? " [RO]" : ""}'

  " Set highlight to User2
  let s .= ' %2* '

  " Right justify
  let s .= '%='

  " Set highlight to User3
  let s .= ' %3* '

  " Show git branch, if available
  let s .= '%{FugitiveStatusLine("' . a:sep . '")}'

  " Show line break style
  let s .= '%{&fileformat ==# "unix" ? "LF ' . a:sep . ' " : ""}'
  let s .= '%{&fileformat ==# "dos" ? "CRLF ' . a:sep . ' " : ""}'
  let s .= '%{&fileformat ==# "mac" ? "CR ' . a:sep . ' " : ""}'

  " Show file type
  let s .= '%{&filetype ==# "" ? "none" : &filetype}'

  " Set highlight to User4
  let s .= ' %4* '

  " Position in file
  let s .= '%(%l:%c%V%) %P '

  return s
endfunction

set laststatus=2
set noshowmode
set statusline=%!StatusLine('/')
