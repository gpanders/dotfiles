" Markdown filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

let g:markdown_interp_languages = ['python', 'sh=bash', 'bash', 'ruby', 'console=bash']

if executable('pandoc')
  compiler pandoc
  setlocal formatprg=pandoc\ -f\ markdown\ -t\ markdown\ --reference-links
  let b:undo_ftplugin .= '|setl fp<'
endif

setlocal textwidth=72
setlocal spell
" https://github.com/tpope/vim-markdown/issues/134
setlocal comments=n:>
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^\\s*[-*+]\\s\\+\\\|^\\[^\\ze[^\\]]\\+\\]:
let b:undo_ftplugin .= '|setl tw< spell< com< flp<'

if has('conceal')
  setlocal conceallevel=2
  let b:undo_ftplugin .= '|setl cole<'
endif

" Use [[ and ]] to navigate between sections
nnoremap <buffer> <silent> [[ :<C-U>for _ in range(v:count1)<Bar>call search('^#', 'bsW')<Bar>endfor<CR>
nnoremap <buffer> <silent> ]] :<C-U>for _ in range(v:count1)<Bar>call search('^#', 'sW')<Bar>endfor<CR>
let b:undo_ftplugin .= '|nun <buffer> [[|nun <buffer> ]]'

nnoremap <buffer> <silent> Z! :<C-U>call ft#markdown#eval()<CR>
let b:undo_ftplugin .= '|nun <buffer> Z!'
