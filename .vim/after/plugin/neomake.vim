if !get(g:, 'loaded_neomake')
    finish
endif

augroup plugin_neomake
    autocmd!
    " Defer configuration until first actual file is opened
    autocmd FileType * ++once call neomake#configure#automake({
                \ 'TextChanged': {},
                \ 'InsertLeave': {},
                \ 'BufWritePost': {'delay': 0},
                \ 'BufWinEnter': {},
                \ }, 500)
augroup END

if !exists('g:neomake')
    let g:neomake = {}
endif

let g:neomake_highlight_columns = 0

let g:neomake_error_sign = { 'text': 'E' }
let g:neomake_warning_sign = { 'text': 'W' }
let g:neomake_message_sign = { 'text': 'M' }
let g:neomake_info_sign = { 'text': 'I' }

let g:neomake_python_enabled_makers = ['pylint', 'mypy']

let g:neomake_c_enabled_makers = ['clangtidy', 'clangcheck']
let g:neomake_cpp_enabled_makers = g:neomake_c_enabled_makers
let g:neomake_cpp_clangtidy_args = ['--checks="cppcoreguidelines-*"']
