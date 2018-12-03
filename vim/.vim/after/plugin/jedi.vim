if exists('plugs') && has_key(plugs, 'jedi-vim')
  let g:jedi#goto_command = "gd"
  let g:jedi#goto_assignments_command = ""
  let g:jedi#usages_command = ""
  let g:jedi#rename_command = "gr"
endif
