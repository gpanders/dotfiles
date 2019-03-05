" Provide a :Scratch command to view output of any ex command in a scratch
" buffer
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-03

if exists('g:loaded_scratch')
  finish
endif
let g:loaded_scratch = 1

function! s:Scratch(command, ...)
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
  0
endfunction

command! -nargs=+ Scratch     call <SID>Scratch(<f-args>)
command! -nargs=? Scriptnames call <SID>Scratch('scriptnames', <f-args>)
command! -nargs=? Oldfiles    call <SID>Scratch('oldfiles', <f-args>)
