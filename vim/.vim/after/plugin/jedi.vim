" jedi-vim configuration
" Author: Greg Anders <greg@gpanders.com>

if exists(':JediDebugInfo') != 2
  finish
endif

let g:jedi#goto_command = "<Bslash>d"
let g:jedi#goto_assignments_command = "<Bslash>a"
let g:jedi#usages_command = "<Bslash>u"
let g:jedi#rename_command = "<Bslash>r"
