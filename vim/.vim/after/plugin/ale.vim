" ALE configuration
" This file is executed AFTER ALE is loaded
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-01-23

if !get(g:, 'loaded_ale', 0)
  finish
endif

" Python
let g:ale_python_pylint_change_directory = 0

" C/C++
let g:ale_c_parse_compile_commands = 1
let g:ale_c_ccls_init_options = {
      \ 'cache': {
      \   'directory': $HOME . '/.cache/ccls',
      \ }}
let g:ale_cpp_ccls_init_options = g:ale_c_ccls_init_options

let g:ale_fixers = {
      \ 'python': ['isort'],
      \ 'cpp': ['clang-format'],
      \ 'c': ['clang-format'],
      \ '*': ['remove_trailing_lines', 'trim_whitespace']
      \}

nnoremap <silent> <Space><C-F> :ALEFix<CR>
nmap <Bslash>d <Plug>(ale_go_to_definition)
nmap <C-W><Bslash>d <Plug>(ale_go_to_definition_in_split)
