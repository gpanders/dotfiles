" Markdown filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

let g:markdown_interp_languages = ['python', 'sh=bash', 'bash', 'ruby', 'console=bash']

setlocal textwidth=79
setlocal spell
" https://github.com/tpope/vim-markdown/issues/134
setlocal comments=n:>
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:
let b:undo_ftplugin .= '|setl tw< spell< com< flp<'

if has('conceal')
  setlocal conceallevel=2
  let b:undo_ftplugin .= '|setl cole<'
endif

if executable('pandoc')
  compiler pandoc
  let &l:formatprg = 'pandoc -f gfm -t gfm --standalone --columns=' . &textwidth
  command! -buffer Toc exe '%!' . &l:formatprg . ' --toc'
  let b:undo_ftplugin .= '|setl fp<|delc Toc'
endif

" Use [[ and ]] to navigate between sections
nnoremap <buffer> <silent> [[ :<C-U>call ft#markdown#section(1)<CR>
nnoremap <buffer> <silent> ]] :<C-U>call ft#markdown#section(0)<CR>
let b:undo_ftplugin .= '|nun <buffer> [[|nun <buffer> ]]'

nnoremap <buffer> <silent> Z! :<C-U>call ft#markdown#eval()<CR>
let b:undo_ftplugin .= '|nun <buffer> Z!'
