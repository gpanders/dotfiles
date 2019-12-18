" vim plugin for notes
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-12-17

if exists('g:loaded_note') || empty($NOTES_DIR)
    finish
endif
let g:loaded_note = 1

augroup plugin.note
    autocmd!
    autocmd BufReadPost $NOTES_DIR/*
                \ setlocal include=\\[\\[\\zs\\d\\+\\ze]] includeexpr=glob(v:fname.'*')
augroup END
