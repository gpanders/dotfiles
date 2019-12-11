" Markdown filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

let g:markdown_interp_languages = ['python', 'sh=bash', 'bash', 'ruby', 'console=bash']

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
  let &l:formatprg = 'pandoc -f markdown -t markdown --atx-headers --standalone --columns=' . &textwidth
  setlocal formatexpr=ft#markdown#formatexpr()
  command! -buffer Toc call ft#markdown#toc()
  command! -bang -buffer Reflinks call ft#markdown#reflinks(<bang>0)
  let b:undo_ftplugin .= '|setl fp< fex<|delc Toc|delc Reflinks'
endif

" Use [[ and ]] to navigate between sections
nnoremap <buffer> <silent> [[ :<C-U>call ft#markdown#section(1)<CR>
nnoremap <buffer> <silent> ]] :<C-U>call ft#markdown#section(0)<CR>
let b:undo_ftplugin .= '|nun <buffer> [[|nun <buffer> ]]'

nnoremap <buffer> <silent> Z! :<C-U>call ft#markdown#eval()<CR>
let b:undo_ftplugin .= '|nun <buffer> Z!'

nnoremap <buffer> <silent> <CR> :<C-U>call ft#markdown#open('edit')<CR>
nnoremap <buffer> <silent> <C-W><CR> :<C-U>call ft#markdown#open('split')<CR>
let b:undo_ftplugin .= '|nun <buffer> <CR>|nun <buffer> <S-CR>'
