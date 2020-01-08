function! s:tagjump()
  let l = getline('.')
  let cmd = matchstr(l, '^[^\t]\+\t\zs.*$')
  wincmd w
  let magic = &magic
  set nomagic
  call cursor(1, 1)
  execute s:escape(cmd)
  let &magic = magic
endfunction

function! s:escape(cmd)
  return substitute(a:cmd, '.\zs/\ze.', '\\/', 'g')
endfunction

function! s:unescape(cmd)
  return substitute(a:cmd, '[^\\]\zs\\/', '/', 'g')
endfunction

function! taglist#open(...)
  let abspath = expand('%:p')
  let relpath = fnamemodify(abspath, ':.')
  if a:0
    let pattern = a:1
  else
    let pattern = '.'
  endif

  let tags = filter(taglist(pattern, abspath),
        \ { _, val -> val.filename ==# relpath })

  let maxtaglen = max(map(copy(tags), 'len(v:val.name)'))
  call map(tags, 'printf("[%s] %s\t%s", v:val.kind, v:val.name, s:unescape(v:val.cmd))')

  " Syntax of current buffer
  let syn = &syntax

  for win in filter(range(1, winnr('$')), 'getwinvar(v:val, "taglist")')
    execute win . 'windo close'
  endfor
  botright 10new
  let w:taglist = 1
  nnoremap <buffer> <silent> <CR> :<C-U>call <SID>tagjump()<CR>
  nnoremap <buffer> q <C-W>c
  silent file Taglist
  call setline(1, tags)
  let &l:tabstop = max([maxtaglen + 10, 40])
  let &l:syntax = syn
  " concealing patterns with matchadd must have priority 0 to avoid conflicts
  " with hlsearch. This seems to be a bug, see:
  " https://github.com/vim/vim/issues/2185
  call matchadd('Conceal', '\t\zs/^\s*', 0)
  call matchadd('Conceal', '$/$', 0)
  call matchadd('Comment', '^\[\w]')
  setlocal nobuflisted buftype=nofile bufhidden=hide noswapfile
  setlocal nomodifiable cursorline conceallevel=2 concealcursor=nc nonumber nowrap
endfunction
