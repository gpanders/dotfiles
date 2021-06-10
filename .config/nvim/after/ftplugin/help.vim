noremap <buffer> <CR> g<C-]>
noremap <silent> <buffer> <C-J> /<Bar>\S\{-}<Bar><CR><Cmd>nohlsearch<CR>
noremap <silent> <buffer> <C-K> ?<Bar>\S\{-}<Bar><CR><Cmd>nohlsearch<CR>

" Make help in vim more like a pager
noremap <nowait> <buffer> q <C-W>q
noremap <nowait> <buffer> d <C-d>
noremap <nowait> <buffer> u <C-u>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')
      \ . '|unm <buffer> <CR>'
      \ . '|unm <buffer> <C-J>'
      \ . '|unm <buffer> <C-K>'
      \ . '|unm <buffer> q'
      \ . '|unm <buffer> d'
      \ . '|unm <buffer> u'
