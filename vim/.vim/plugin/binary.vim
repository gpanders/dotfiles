" Improved editing of binary files
" This makes vim a slightly more tolerable hex editor, although there are
" still better options out there
" Most of the magic happens in the xxd filetype plugin, not here
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-24

augroup plugin.binary
  autocmd!
  autocmd BufReadPost  * if &bin | exe '%!xxd' | set ft=xxd | endif
  autocmd BufWritePre  * if &bin | exe '%!xxd -r' | endif
  autocmd BufWritePost * if &bin | exe '%!xxd' | set nomod | endif
augroup END

" Delete the augroup if not in binary mode
au VimEnter * if !&binary | exe 'au! plugin.binary' | endif
