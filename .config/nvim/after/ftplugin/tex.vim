setlocal nocursorline
setlocal norelativenumber
" :NoMatchParen
let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl cul< rnu<'

if executable('latexindent')
    setlocal equalprg=latexindent
    let b:undo_ftplugin .= '|setl ep<'
endif
