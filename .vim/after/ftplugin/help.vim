let b:undo_ftplugin = get(b:, 'undo_ftplugin')

noremap <silent> <buffer> <CR> g<C-]>
noremap <silent> <buffer> <C-J> /<Bar>\S\{-}<Bar><CR>:nohlsearch<CR>
noremap <silent> <buffer> <C-K> ?<Bar>\S\{-}<Bar><CR>:nohlsearch<CR>

" Make help in vim more like a pager
noremap <silent> <nowait> <buffer> q :q<CR>
noremap <silent> <nowait> <buffer> d <C-d>
noremap <silent> <nowait> <buffer> u <C-u>

let b:undo_ftplugin = b:undo_ftplugin
      \ . '|unm <buffer> <CR>'
      \ . '|unm <buffer> <C-J>'
      \ . '|unm <buffer> <C-K>'
      \ . '|unm <buffer> q'
      \ . '|unm <buffer> d'
      \ . '|unm <buffer> u'
