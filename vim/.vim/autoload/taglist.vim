function! s:tagjump()
  let view = winsaveview()
  let l = getline('.')
  let pattern = matchstr(l, '^.\{-}\t/\zs.\{-}\ze/$')
  wincmd w
  call cursor(1, 1)
  call search(pattern, 'c')
  wincmd w
  call winrestview(view)
endfunction

function! taglist#open()
  let abspath = expand('%:p')
  let relpath = fnamemodify(abspath, ':.')
  let tags = filter(copy(taglist('.', abspath)),
        \ { _, val -> val.filename ==# relpath })

  let maxtaglen = max(map(copy(tags), { _, val -> len(val.name) }))
  let text = map(tags, { _, val ->
        \ '[' . val.kind . '] ' .
        \ val.name . "\t" . val.cmd })

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
    setlocal cursorline conceallevel=2 concealcursor=nc
    nnoremap <buffer> <silent> <CR> :<C-U>call <SID>tagjump()<CR>
    nnoremap <buffer> q <C-W>c
    10wincmd_
    execute 'silent file Taglist'
  endif
  let &l:tabstop = max([maxtaglen + 10, 30])
  put =tags
  0delete_
  let &l:syntax = syn
  call matchadd('Conceal', '\t\zs/^\s*')
  call matchadd('Conceal', '$/$')
  call matchadd('Comment', '^\[\w]')
  setlocal nomodifiable
endfunction
