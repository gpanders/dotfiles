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
  let &l:formatprg = 'pandoc -f markdown -t markdown -s --reference-links --reference-location=section --columns=' . &textwidth
  command! -buffer Toc call ft#markdown#toc()
  command! -bang -buffer Reflinks call ft#markdown#setopt('reference-links', <bang>0)
  command! -bang -buffer Setext call ft#markdown#setopt('atx-headers', <bang>1)
  let b:undo_ftplugin .= '|setl fp<|delc Toc|delc Reflinks|delc Setext'
endif

nnoremap <buffer> <silent> Z! :<C-U>EvalBlock<CR>
let b:undo_ftplugin .= '|nun <buffer> Z!'

nnoremap <buffer> <silent> <CR> :<C-U>call ft#markdown#open('edit')<CR>
nnoremap <buffer> <silent> <C-W><CR> :<C-U>call ft#markdown#open('split')<CR>
let b:undo_ftplugin .= '|nun <buffer> <CR>|nun <buffer> <C-W><CR>'
