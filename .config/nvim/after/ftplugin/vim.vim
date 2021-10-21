let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal foldmethod=marker
setlocal foldmarker=\ {{{,\ }}}
setlocal formatoptions-=r
let b:undo_ftplugin .= '|setl fdm< fmr< fo<'
