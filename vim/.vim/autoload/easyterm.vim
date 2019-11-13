" Re-use a terminal buffer window for the same command
" Author: Greg Anders

" Create a terminal buffer using `cmd` as the command to run. If this function
" is called again with the same `cmd` argument, re-open the existing buffer
" instead of creating another new buffer. This is useful for terminal buffers
" that should persist (e.g. REPLs)
function easyterm#open(mods, ...)
  if a:0 && !empty(a:1)
    let cmd = a:1
  else
    let cmd = &shell
  endif

  let name = split(cmd)[0]
  let mods = empty(a:mods) ? 'botright' : a:mods

  let buf = filter(range(1, bufnr('$')),
        \ 'getbufvar(v:val, "&buftype") ==# "terminal" && getbufvar(v:val, "' . name . '", 0)')
  if empty(buf)
    " No buffer yet, so start a new one
    if has('nvim')
      execute mods . ' sp | term ' . cmd
    else
      execute mods . ' term ' . cmd
    endif
    let b:{name} = 1
  else
    " Buffer already exists
    let bufinfo = getbufinfo(buf[0])[0]
    if !bufinfo.hidden && !empty(bufinfo.windows)
      let winnum = win_id2win(bufinfo.windows[0])
      execute winnum 'wincmd w'
    else
      execute mods . ' sb ' . bufinfo.bufnr
    endif
  endif
endfunction
