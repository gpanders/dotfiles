if exists('b:current_syntax')
    finish
endif

runtime! syntax/sh.vim syntax/sh_*.vim syntax/sh/*.vim
