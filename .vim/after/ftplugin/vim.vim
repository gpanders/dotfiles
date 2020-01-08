" Vim

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

if has('folding')
  setlocal foldmethod=marker
  setlocal foldmarker=\ {{{,\ }}}
  let b:undo_ftplugin .= '|setl fdm< fmr<'
endif

setlocal formatoptions-=r
let b:undo_ftplugin .= '|setl fo<'

" Z: execute current line or visually selected region in command mode
nnoremap <buffer> Z: ^"zyg_:<C-R>z<CR>
vnoremap <buffer> Z: "zy:<C-U><C-R>=substitute(@z, "\n", "<Bar>", "g")<CR><BS>

" Source current file with c<Enter>
nnoremap <silent> <buffer> c<CR> :source %<Bar>redraw!<CR>

let b:undo_ftplugin .= '|unm <buffer> Z:|unm <buffer> c<CR>'
