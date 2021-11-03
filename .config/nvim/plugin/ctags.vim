function! s:on_exit(_, code, ...)
    if a:code > 0
        echohl WarningMsg
        echo 'ctags failed with code ' . a:code
        echohl None
    endif
endfunction

command! -nargs=* Ctags call jobstart('ctags -R ' . <q-args>, {'on_exit': function('s:on_exit')})
