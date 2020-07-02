let s:save_cpo = &cpo
set cpo&vim

if !get(g:, 'loaded_ale')
    finish
endif

if !exists('g:ale_linters')
    let g:ale_linters = {}
endif

if !exists('g:ale_fixers')
    let g:ale_fixers = {}
endif

augroup plugin_ale | exec 'autocmd!' | augroup END

if get(g:, 'ale_completion_enabled')
    let cot = &completeopt
    set completeopt=menu,menuone,preview,noselect,noinsert
    if cot =~# 'popup'
        set completeopt-=preview completeopt+=popup
    endif
endif

if empty(&completefunc)
    setglobal completefunc=ale#completion#OmniFunc
endif

" Python
let g:ale_linters.python = ['pylint', 'flake8', 'pyls', 'mypy']
let g:ale_fixers.python = ['black', 'isort']
let g:ale_python_black_change_directory = 0
let g:ale_python_pylint_change_directory = 0
let g:ale_python_flake8_change_directory = 0
let g:ale_python_mypy_ignore_invalid_syntax = 1
let g:ale_python_mypy_options = '--ignore-missing-imports'
let g:ale_python_pyls_config = {
            \ 'pyls': {
            \   'configurationSources': ['flake8'],
            \   'plugins': {
            \     'pycodestyle': {
            \       'enabled': v:false,
            \     },
            \   },
            \ }}

" C/C++
let g:ale_linters.c = ['clangd']
let g:ale_linters.cpp = g:ale_linters.c


let g:ale_c_clangd_options = '--clang-tidy --fallback-style=LLVM --background-index --suggest-missing-includes --header-insertion=iwyu'
let g:ale_cpp_clangd_options = g:ale_c_clangd_options . ' --clang-tidy-checks=cppcoreguidelines-*'

let g:ale_c_parse_makefile = 1
let g:ale_c_parse_compile_commands = 1

" VHDL
let g:ale_vhdl_xvhdl_options = '--2008 --nolog'

" Rust
let g:ale_linters.rust = ['cargo', 'rls']
let g:ale_rust_rls_toolchain = 'stable'

" Go
let g:ale_linters.go = ['golint', 'gofmt', 'gopls']
let g:ale_fixers.go = ['gofmt']

" sh
let g:ale_fixers.sh = ['shfmt']
let g:ale_sh_shfmt_base_options = '-s'
autocmd plugin_ale FileType sh let g:ale_sh_shfmt_options = g:ale_sh_shfmt_base_options . ' -ln=' . (get(b:, 'is_bash') ? 'bash' : 'posix')

function! s:completion()
    if !ale#completion#GetCompletions()
        return
    endif

    let b:omnifunc_orig = &omnifunc
    setlocal omnifunc=ale#completion#OmniFunc
    nmap <buffer> <C-]> <Plug>(ale_go_to_definition)
    nmap <buffer> <C-W>] <Plug>(ale_go_to_definition_in_split)
    nmap <buffer> <C-W><C-]> <Plug>(ale_go_to_definition_in_split)
    nmap <buffer> gr <Plug>(ale_find_references)
    nnoremap <silent> <buffer> gR :<C-U>ALERename<CR>
    nmap <buffer> K <Plug>(ale_hover)

    let b:undo_ale_completion = 'setl ofu=' . b:omnifunc_orig
                \ . '|nun <buffer> <C-]>'
                \ . '|nun <buffer> <C-W>]'
                \ . '|nun <buffer> <C-W><C-]>'
                \ . '|nun <buffer> gr'
                \ . '|nun <buffer> gR'
                \ . '|nun <buffer> K'

    unlet b:omnifunc_orig
endfunction

function! s:toggle()
    if !g:ale_enabled
        return
    endif

    if !&modifiable || &readonly
        silent ALEDisableBuffer
        if exists('b:undo_ale_completion')
            exec b:undo_ale_completion
            unlet b:undo_ale_completion
        endif
    elseif !get(b:, 'ale_enabled', 1)
        silent ALEEnableBuffer
        call <SID>completion()
    endif
endfunction

augroup plugin_ale
    autocmd OptionSet modifiable,readonly call <SID>toggle()
    autocmd BufWinEnter * call <SID>toggle()
    autocmd FileType * call <SID>completion()

    " Use ALE to format whole buffer if fixers are enabled for the current
    " filetype
    autocmd FileType * if has_key(g:ale_fixers, &filetype) | exec 'nmap gq<CR> <Plug>(ale_fix)' | endif
augroup END

let &cpo = s:save_cpo
unlet s:save_cpo
