function pyterm#open(...)
  let buf = filter(range(1, bufnr('$')),
        \ 'bufexists(v:val) && getbufvar(v:val, "pyterm", 0)')
  let height = winheight(0) / 3
  if empty(buf)
    " No buffer yet, so start a new one
    if a:0
      let pyprog = a:1
    elseif executable('ipython')
      let pyprog = 'ipython'
    elseif executable('python')
      let pyprog = 'python'
    elseif executable('python3')
      let pyprog = 'python3'
    elseif executable('python2')
      let pyprog = 'python2'
    endif

    if has('nvim')
      execute 'botright ' . height . 'sp | term ' . pyprog
      autocmd BufEnter <buffer> startinsert
    else
      execute 'botright term ' . pyprog
      execute 'resize ' . height
      autocmd BufEnter <buffer> normal i
    endif
    setlocal nonumber nobuflisted
    let b:pyterm = 1
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
