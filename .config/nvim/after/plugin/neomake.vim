if !get(g:, 'loaded_neomake')
    finish
endif

augroup my_neomake
    autocmd!
    " Defer configuration until first actual file is opened
    autocmd FileType * ++once call neomake#configure#automake('w')

    " Disable Neomake for LSP buffers
    autocmd User LspAttached silent NeomakeDisableBuffer | NeomakeClean
augroup END


if !exists('g:neomake')
    let g:neomake = {}
endif

let g:neomake_highlight_columns = 0

let g:neomake_error_sign = { 'text': 'E' }
let g:neomake_warning_sign = { 'text': 'W' }
let g:neomake_message_sign = { 'text': 'M' }
let g:neomake_info_sign = { 'text': 'I' }

let g:neomake_python_enabled_makers = []

if executable('flake8')
    let g:neomake_python_enabled_makers += ['flake8']
elseif executable('pylint')
    let g:neomake_python_enabled_makers += ['pylint']
endif

if executable('mypy')
    let g:neomake_python_enabled_makers += ['mypy']
endif

let g:neomake_c_enabled_makers = []

if executable('clang-tidy')
    let g:neomake_c_enabled_makers += ['clangtidy']
endif

if executable('clang-check')
    let g:neomake_c_enabled_makers += ['clangcheck']
endif

let g:neomake_cpp_enabled_makers = g:neomake_c_enabled_makers

let g:neomake_cpp_clangtidy_args = ['--checks="cppcoreguidelines-*"']

let g:neomake_rust_cargo_command = ['clippy']
