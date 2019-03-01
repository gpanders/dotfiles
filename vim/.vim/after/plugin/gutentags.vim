" vim-gutentags configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-03

if !get(g:, 'loaded_gutentags', 0)
  finish
endif

if executable('rg')
  let g:gutentags_file_list_command = 'rg --files'
elseif executable('ag')
  let g:gutentags_file_list_command = 'ag -l'
endif

" Don't create tags for git projects since they are created in hooks
" https://tbaggery.com/2011/08/08/effortless-ctags-with-git.html
call remove(g:gutentags_project_root, index(g:gutentags_project_root, '.git'))
