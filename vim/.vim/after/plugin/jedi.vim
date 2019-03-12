" jedi-vim configuration
" Author: Greg Anders <greg@gpanders.com>

if exists(':JediDebugInfo') != 2
  finish
endif

let g:jedi#smart_auto_mappings = 0
let g:jedi#goto_command = "gd"
let g:jedi#goto_assignments_command = ""
let g:jedi#usages_command = ""
let g:jedi#rename_command = "gr"
