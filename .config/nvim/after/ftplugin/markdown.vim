let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

setlocal textwidth=79
" https://github.com/tpope/vim-markdown/issues/134
setlocal comments=n:>
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:
let b:undo_ftplugin .= '|setl tw< com< flp<'

if has('conceal')
  setlocal conceallevel=2
  let b:undo_ftplugin .= '|setl cole<'
endif

if executable('pandoc')
  compiler pandoc
endif

nnoremap <buffer> <silent> Z! :<C-U>EvalBlock<CR>
let b:undo_ftplugin .= '|nun <buffer> Z!'

nnoremap <buffer> <silent> <CR> :<C-U>call ft#markdown#open('edit')<CR>
nnoremap <buffer> <silent> <C-W><CR> :<C-U>call ft#markdown#open('split')<CR>
let b:undo_ftplugin .= '|nun <buffer> <CR>|nun <buffer> <C-W><CR>'
