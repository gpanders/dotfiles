" Re-use a terminal buffer window for the same command
" Author: Greg Anders

" Create a terminal buffer using `cmd` as the command to start and an optional
" name as the second argument. If this function is called again with the same
" `cmd` / `name` argument, re-open the existing buffer instead of creating
" another new buffer. This is useful for terminal buffers that should persist
" (e.g. REPLs)
function easyterm#open(cmd, ...)
  if a:0 && !empty(a:1)
    let name = a:1
  else
    let name = a:cmd
  endif

  let buf = filter(range(1, bufnr('$')),
        \ 'bufexists(v:val) && getbufvar(v:val, "' . name . '", 0)')
  let height = winheight(0) / 3
  if empty(buf)
    " No buffer yet, so start a new one
    if has('nvim')
      execute 'botright ' . height . 'sp | term ' . a:cmd
    else
      execute 'botright term ' . a:cmd
      execute 'resize ' . height
    endif
    let b:{name} = 1
  else
    " Buffer already exists
    let bufinfo = getbufinfo(buf[0])[0]
    if !bufinfo.hidden && !empty(bufinfo.windows)
      let winnum = win_id2win(bufinfo.windows[0])
      execute winnum 'wincmd w'
    else
      execute 'botright sb ' . bufinfo.bufnr . ' | resize ' . height
    endif
  endif
endfunction
