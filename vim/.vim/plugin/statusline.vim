" Set the statusline

let s:statusline_separator = ' / '

function! StatuslineLineEnding()
  if &ff ==# 'unix'
    return 'LF' . s:statusline_separator
  elseif &ff ==# 'dos'
    return 'CRLF' . s:statusline_separator
  elseif &ff ==# 'mac'
    return 'CR' . s:statusline_separator
  endif
  return ''
endfunction

function! StatuslineFileType()
  if &ft ==# ''
    return 'none'
  else
    return &ft
  endif
endfunction

function! StatuslineGitBranch()
  if exists('*FugitiveHead')
    let branch = FugitiveHead()
    if branch != ''
      return branch . s:statusline_separator
    endif
  endif
  return ''
endfunction

function! StatuslineMode()
  let m = mode()
  if m ==# 'n'
    highlight! link StatuslineMode StatuslineModeNormal
    return 'N'
  elseif m ==# 'i'
    highlight! link StatuslineMode StatuslineModeInsert
    return 'I'
  elseif m ==# 'R'
    highlight! link StatuslineMode StatuslineModeReplace
    return 'R'
  elseif m ==? 'v' || m ==# ''
    highlight! link StatuslineMode StatuslineModeVisual
    return 'V'
  else
    highlight! link StatuslineMode StatuslineModeNormal
    return toupper(m)
  endif
endfunction

" Initialize StatuslineMode highlight group
highlight! link StatuslineMode StatuslineModeNormal

function! StatuslineFlags()
  let str = ''
  if &mod
    let str .= '+ '
  endif
  if &ro
    let str .= '[RO] '
  endif
  return str
endfunction

set laststatus=2
let &statusline = ''                            " Reset the statusline
let &statusline .= '%#StatuslineMode#'          " Set color to StatuslineMode
let &statusline .= ' %{StatuslineMode()} '      " Mode indicator
let &statusline .= '%*'                         " Reset color
let &statusline .= '%1*'                        " Set color to User1
let &statusline .= ' %<%f %{StatuslineFlags()}' " Filename and flags
let &statusline .= '%2*'                        " Set color to User2
let &statusline .= '%='                         " Break point for right justify

let &statusline .= '%3*'                        " Set color to User3
let &statusline .= '%{StatuslineGitBranch()}'   " Git branch
let &statusline .= '%{StatuslineLineEnding()}'  " Line ending
let &statusline .= '%{StatuslineFileType()}  '  " Filetype
let &statusline .= '%4*'                        " Set color to User4
let &statusline .= ' %(%l:%c%V%) %P '           " Position in file
