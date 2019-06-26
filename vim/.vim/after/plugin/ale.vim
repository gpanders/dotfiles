" ALE configuration
" This file is executed AFTER ALE is loaded
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-01-23

let s:save_cpo = &cpo
set cpo&vim

if !get(g:, 'loaded_ale', 0)
  finish
endif

if !exists('g:ale_linters')
  let g:ale_linters = {}
endif

" Python
let g:ale_linters.python = ['pylint', 'flake8', 'pyls']
let g:ale_python_pylint_change_directory = 0
let g:ale_python_flake8_change_directory = 0
let g:ale_python_pyls_config = {'pyls': {'configurationSources': ['flake8', 'pylint']}}

" C/C++
let g:ale_linters.c = ['ccls', 'cquery', 'clangtidy']
let g:ale_linters.cpp = g:ale_linters.c

let g:ale_c_parse_makefile = 1
let g:ale_c_parse_compile_commands = 1
let g:ale_c_ccls_init_options = {
      \ 'cache': {
      \   'directory': $HOME . '/.cache/ccls',
      \ }}

let g:ale_cpp_ccls_init_options = g:ale_c_ccls_init_options
let g:ale_cpp_clangtidy_checks = ['cppcoreguidelines-*']

" VHDL
let g:ale_vhdl_xvhdl_options = '--2008 --nolog'

let g:ale_fixers = {
      \ 'python': ['isort'],
      \ 'cpp': ['clang-format'],
      \ 'c': ['clang-format'],
      \ '*': ['remove_trailing_lines', 'trim_whitespace']
      \}

imap <C-Space> <Plug>(ale_complete)
nmap <Space><C-F> <Plug>(ale_fix)
nmap <Bslash>d <Plug>(ale_go_to_definition)
nmap <C-W><Bslash>d <Plug>(ale_go_to_definition_in_split)
nmap <Bslash>r <Plug>(ale_find_references)

function! s:toggle()
  if !&modifiable || &readonly
    silent ALEDisableBuffer
  else
    silent ALEEnableBuffer
  endif
endfunction

augroup plugin.ale
  autocmd!
  autocmd OptionSet modifiable,readonly call <SID>toggle()
  autocmd BufReadPost * call <SID>toggle()
augroup END

let &cpo = s:save_cpo
unlet s:save_cpo
