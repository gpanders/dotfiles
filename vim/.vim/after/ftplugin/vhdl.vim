" VHDL filetype plugin
" Author: Greg Anders <greg@gpanders.com>

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

" VHDL uses -- as comment string
setlocal comments=:--
setlocal commentstring=--%s

" VHDL is case insensitive
setlocal tagcase=ignore
setlocal ignorecase
setlocal nosmartcase
setlocal formatoptions-=t
setlocal textwidth=79

setlocal include=\\c^\\s*use\\s\\+work\\.\\zs\\w\\+\\ze\\.
setlocal includeexpr=ft#vhdl#find_package(v:fname)

inoreabbrev <buffer> slv std_logic_vector
inoreabbrev <buffer> sl std_logic

nnoremap <buffer> <silent> <Bslash>d :<C-U>call ft#vhdl#toggle_debug()<CR>

if executable('ghdl')
  compiler ghdl
endif

let b:undo_ftplugin .=
      \ '|setl com< cms< tc< ic< scs< fo< tw< inc< inex<' .
      \ '|iuna <buffer> slv' .
      \ '|iuna <buffer> sl' .
      \ '|nun <buffer> <Bslash>d'
