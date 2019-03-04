" ALE configuration
" This file is executed AFTER ALE is loaded
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-01-23

if !get(g:, 'loaded_ale', 0)
  finish
endif

let g:ale_python_pylint_change_directory = 0

let g:ale_fixers = {
      \ 'python': ['isort'],
      \ 'cpp': ['clang-format'],
      \ 'c': ['clang-format'],
      \ '*': ['remove_trailing_lines', 'trim_whitespace']
      \}

nnoremap <silent> <Space><C-F> :ALEFix<CR>
