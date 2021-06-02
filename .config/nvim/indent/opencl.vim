if exists('b:did_indent')
    finish
endif

runtime! indent/c.vim indent/c_*.vim indent/c/*.vim
