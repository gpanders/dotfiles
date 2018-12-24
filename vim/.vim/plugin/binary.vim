augroup Binary
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
au VimEnter * if !&binary | au! Binary | endif
