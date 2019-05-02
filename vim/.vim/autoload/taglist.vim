function! s:tagjump()
  let view = winsaveview()
  let l = getline('.')
  let pattern = matchstr(l, '^.\{-}\t/\zs.\{-}\ze/$')
  wincmd w
  call cursor(1, 1)
  call search('\M' . pattern, 'c')
  wincmd w
  call winrestview(view)
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

  let maxtaglen = max(map(copy(tags), { _, val -> len(val.name) }))
  let text = map(tags, { _, val ->
        \ '[' . val.kind . '] ' . val.name . "\t" . val.cmd })

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
    setlocal nobuflisted
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    nnoremap <buffer> <silent> <CR> :<C-U>call <SID>tagjump()<CR>
    nnoremap <buffer> q <C-W>c
    10wincmd_
    execute 'silent file Taglist'
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
  setlocal nomodifiable
  setlocal cursorline
  setlocal conceallevel=2
  setlocal concealcursor=nc
  setlocal nonumber
endfunction
