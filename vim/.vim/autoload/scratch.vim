function! scratch#open(command, ...)
  redir => lines
  let more = &more
  set nomore
  execute a:command
  redir END
  let &more = more
  call feedkeys("\<CR>")
  let lines = substitute(lines, '\n\zs\s*\d\+:\s*', '', 'g')
  new
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile
  put =lines
  let b:lines = lines
  nnoremap <buffer> q <C-W>q
  nnoremap <buffer> R :%d_<Bar>pu =b:lines<Bar>%g/^\s*$/d<Bar>0<CR>
  nnoremap <buffer> <CR> gf<C-W>o
  if a:0 > 0
    execute 'vglobal/' . a:1 . '/delete'
  endif
  silent %substitute/\%^\_s*\n\|\_s*\%$
  let height = min([line('$'), 20])
  execute height . 'wincmd_'
  0
endfunction
