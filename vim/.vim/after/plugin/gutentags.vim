if executable('rg')
  let g:gutentags_file_list_command = 'rg --files'
elseif executable('ag')
  let g:gutentags_file_list_command = 'ag -l'
endif

call remove(g:gutentags_project_root, index(g:gutentags_project_root, '.git'))
