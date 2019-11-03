" ALE configuration
" This file is executed AFTER ALE is loaded
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-01-23

let s:save_cpo = &cpo
set cpo&vim

if !get(g:, 'loaded_ale')
    finish
endif

if !exists('g:ale_linters')
    let g:ale_linters = {}
endif

" Python {{{
let g:ale_linters.python = ['pylint', 'flake8', 'pyls']
let g:ale_python_pylint_change_directory = 0
let g:ale_python_flake8_change_directory = 0
let g:ale_python_pyls_config = {
            \ 'pyls': {
            \   'configurationSources': ['flake8', 'pylint'],
            \   'plugins': {
            \     'pycodestyle': {
            \       'enabled': v:false,
            \     },
            \   },
            \ }}
" }}}

" C/C++ {{{
let g:ale_linters.c = ['clangd']
let g:ale_linters.cpp = g:ale_linters.c


let g:ale_c_clangd_options = '--clang-tidy --fallback-style=LLVM --background-index --suggest-missing-includes --header-insertion=iwyu'
let g:ale_cpp_clangd_options = g:ale_c_clangd_options . ' --clang-tidy-checks=cppcoreguidelines-*'

let g:ale_c_parse_makefile = 1
let g:ale_c_parse_compile_commands = 1

" }}}

" VHDL {{{
let g:ale_vhdl_xvhdl_options = '--2008 --nolog'
" }}}

" Rust {{{
let g:ale_linters.rust = ['cargo', 'rls']
let g:ale_rust_rls_toolchain = 'stable'

let g:ale_fixers = {
            \ 'python': ['isort'],
            \ 'cpp': ['clang-format'],
            \ 'c': ['clang-format'],
            \ '*': ['remove_trailing_lines', 'trim_whitespace']
            \}
" }}}

nmap <Space><C-F> <Plug>(ale_fix)
nmap <Bslash>gd <Plug>(ale_go_to_definition)
nmap <C-W><Bslash>d <Plug>(ale_go_to_definition_in_split)
nmap <Bslash>gr <Plug>(ale_find_references)

if get(g:, 'ale_completion_enabled')
    " See :h ale-completion-completeopt-bug
    set completeopt=menu,menuone,preview,noselect,noinsert
endif

function! s:lsp_setup()
    let buf = bufnr('')
    let lsps = filter(ale#linter#Get(&filetype), {_, v -> 
                \ !empty(v.lsp) && ale#engine#IsExecutable(buf, ale#linter#GetExecutable(buf, v))})
    if empty(lsps)
        return
    endif

    let b:ale_lsp_enabled = 1
    setlocal omnifunc=ale#completion#OmniFunc
    nnoremap <buffer> <C-]> :<C-U>ALEGoToDefinition<CR>
    nnoremap <buffer> <C-W>] :<C-U>ALEGoToDefinitionInSplit<CR>
    nnoremap <buffer> <C-W><C-]> :<C-U>ALEGoToDefinitionInSplit<CR>
    nnoremap <buffer> <Bslash>r :<C-U>ALEFindReferences<CR>
endfunction

function! s:toggle(...)
    if !&modifiable || &readonly
        silent ALEDisableBuffer
    elseif get(a:, '1', 1)
        silent ALEEnableBuffer
    endif
endfunction

augroup plugin.ale
    autocmd!
    autocmd OptionSet modifiable,readonly call <SID>toggle()
    autocmd BufWinEnter * call <SID>toggle(0)
    autocmd FileType * call <SID>lsp_setup()
augroup END

let &cpo = s:save_cpo
unlet s:save_cpo
