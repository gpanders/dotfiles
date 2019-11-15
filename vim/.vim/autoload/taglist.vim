function! s:tagjump()
  let l = getline('.')
  let cmd = matchstr(l, '^[^\t]\+\t\zs.*$')
  wincmd w
  let magic = &magic
  set nomagic
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

  let bufinfo = getbufinfo('Taglist')
  if !empty(bufinfo)
    " Buffer already exists
    let bufinfo = bufinfo[0]
    if !bufinfo.hidden && !empty(bufinfo.windows)
      let winnum = win_id2win(bufinfo.windows[0])
      execute winnum . 'wincmd w'
    else
      execute 'botright sb ' . bufinfo.bufnr . ' | resize 10'
    endif
    setlocal modifiable
    %delete_
  else
    botright new
    setlocal nobuflisted buftype=nofile bufhidden=hide noswapfile
    nnoremap <buffer> <silent> <CR> :<C-U>call <SID>tagjump()<CR>
    nnoremap <buffer> q <C-W>c
    10wincmd_
    silent execute 'file Taglist'
  endif
  let &l:tabstop = max([maxtaglen + 10, 40])
  put =tags
  0delete_
  let &l:syntax = syn
  " concealing patterns with matchadd must have priority 0 to avoid conflicts
  " with hlsearch. This seems to be a bug, see:
  " https://github.com/vim/vim/issues/2185
  call matchadd('Conceal', '\t\zs/^\s*', 0)
  call matchadd('Conceal', '$/$', 0)
  call matchadd('Comment', '^\[\w]')
  setlocal nomodifiable cursorline conceallevel=2 concealcursor=nc nonumber nowrap
endfunction
