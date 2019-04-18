" VHDL filetype plugin
" Author: Greg Anders <greg@gpanders.com>

" VHDL uses -- as comment string
setlocal comments=:--
setlocal commentstring=--%s

" Use two spaces for tabs
setlocal softtabstop=2
setlocal shiftwidth=2

" VHDL is case insensitive
setlocal tagcase=ignore
setlocal ignorecase
setlocal nosmartcase
setlocal formatoptions-=t
setlocal textwidth=79

inoreabbrev <buffer> slv std_logic_vector
inoreabbrev <buffer> sl std_logic

if executable('ghdl')
  compiler ghdl
endif

let b:undo_ftplugin =
      \ 'setl com< cms< sts< sw< tc< ic< scs< fo< tw<' .
      \ '|iuna <buffer> slv' .
      \ '|iuna <buffer> sl'
