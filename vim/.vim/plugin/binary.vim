" Improved editing of binary files
" This makes vim a slightly more tolerable hex editor, although there are
" still better options out there
" Most of the magic happens in the xxd filetype plugin, not here
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-24

augroup plugin.binary
  autocmd!
  autocmd BufReadPost  * if &bin | %!xxd
  autocmd BufReadPost  * set ft=xxd
  autocmd BufReadPost  * endif
  autocmd BufWritePre  * if &bin | %!xxd -r
  autocmd BufWritePre  * endif
  autocmd BufWritePost * if &bin | %!xxd
  autocmd BufWritePost * set nomod | endif
augroup END

" Delete the augroup if not in binary mode
au VimEnter * if !&binary | exe 'au! plugin.binary' | endif
