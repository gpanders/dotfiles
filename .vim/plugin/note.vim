if exists('g:loaded_note') || empty($NOTES_DIR)
    finish
endif
let g:loaded_note = 1

augroup plugin.note
    autocmd!
    autocmd BufReadPost $NOTES_DIR/*
                \ setlocal include=\\[\\[\\zs\\d\\+\\ze]] includeexpr=glob(v:fname.'*')
augroup END
