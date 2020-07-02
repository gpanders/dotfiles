if !get(g:, 'loaded_neomake')
    finish
endif

call neomake#configure#automake({
            \ 'TextChanged': {},
            \ 'InsertLeave': {},
            \ 'BufWritePost': {'delay': 0},
            \ 'BufWinEnter': {},
            \ }, 500)

let g:neomake_error_sign = { 'text': 'E' }
let g:neomake_warning_sign = { 'text': 'W' }
let g:neomake_message_sign = { 'text': 'M' }
let g:neomake_info_sign = { 'text': 'I' }

let g:neomake_python_enabled_makers = ['pylint', 'mypy']
