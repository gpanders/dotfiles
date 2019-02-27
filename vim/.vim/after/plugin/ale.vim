" ALE configuration
" This file is executed AFTER ALE is loaded
" Author: Greg Anders

if !exists('g:loaded_ale')
  finish
endif

let g:ale_python_pylint_change_directory = 0

let g:ale_fixers = {
      \ 'python': ['isort', 'yapf', 'add_blank_lines_for_python_control_statements'],
      \ '*': ['remove_trailing_lines', 'trim_whitespace']
      \}

nnoremap <Space><C-F> :ALEFix<CR>
