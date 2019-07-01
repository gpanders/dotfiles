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
set statusline=                             | " Reset the statusline
set statusline+=%#StatuslineMode#           |
set statusline+=\ %{StatuslineMode()}\      | " Mode indicator
set statusline+=%*                          |
set statusline+=%1*\ %<%f\                  | " Filename
set statusline+=%{StatuslineFlags()}        | " Help/modified/RO markers
set statusline+=%2*                         | " Filetype
set statusline+=%=                          | " Break point for right justify

set statusline+=%3*                         | " Reset color
set statusline+=%{StatuslineGitBranch()}    | " Git branch (with icon)
set statusline+=%{StatuslineLineEnding()}   | " Line ending
set statusline+=%{StatuslineFileType()}\    | " Filetype
set statusline+=%4*\                        | " Color boundary
set statusline+=%(%l:%c%V%)                 | " Line and column number
set statusline+=\ %5*                       | " Color boundary 5
set statusline+=%P\                         | " Percent through file
