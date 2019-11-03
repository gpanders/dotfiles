" go filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

if executable('gofmt')
    setlocal formatprg=gofmt
    let b:undo_ftplugin .= '|setl fp<'
endif
