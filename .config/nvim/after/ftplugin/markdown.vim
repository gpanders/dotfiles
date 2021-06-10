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

if exists(':EvalBlock') == 2
  nnoremap <buffer> Z! <Cmd>EvalBlock<CR>
  let b:undo_ftplugin .= '|nun <buffer> Z!'
endif

nnoremap <buffer> <CR> <Cmd>call ft#markdown#open('edit')<CR>
nnoremap <buffer> <C-W><CR> <Cmd>call ft#markdown#open('split')<CR>
let b:undo_ftplugin .= '|nun <buffer> <CR>|nun <buffer> <C-W><CR>'
