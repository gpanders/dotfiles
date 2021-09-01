augroup restore_cursor
    autocmd! BufRead * if &ft !~# 'commit\|rebase' | exec 'silent! normal! g`"' | endif
augroup END
