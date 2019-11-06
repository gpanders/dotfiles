" go filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal noexpandtab
setlocal shiftwidth=8
setlocal softtabstop=8

let b:undo_ftplugin .= '|setl et< sw< sts<'

if executable('gofmt')
    setlocal formatprg=gofmt
    let b:undo_ftplugin .= ' fp<'
endif
