function! scratch#open(command, ...)
  let lines = substitute(execute(a:command), '\n\zs\s*\d\+:\s*', '', 'g')
  new
  put =lines
  let b:lines = lines
  nnoremap <silent> <buffer> q <C-W>q
  if a:0
    execute 'vglobal/' . a:1 . '/delete'
  endif
  silent %substitute/\%^\_s*\n\|\_s*\%$
  let height = min([line('$'), 20])
  execute height . 'wincmd_'
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile
  0
endfunction
