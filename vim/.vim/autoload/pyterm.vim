function pyterm#open(...)
  let bufinfolist = getbufinfo('term://*python')
  let height = winheight(0) / 3
  if empty(bufinfolist)
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

    execute 'botright ' . height . 'sp | term ' . pyprog
  else
    " Buffer already exists
    let bufinfo = bufinfolist[0]
    if !bufinfo.hidden && !empty(bufinfo.windows)
      let winnum = win_id2win(bufinfo.windows[0])
      execute winnum 'wincmd w | startinsert'
    else
      execute 'botright sb ' . bufinfo.bufnr . ' | resize ' . height . ' | startinsert'
    endif
  endif
endfunction
