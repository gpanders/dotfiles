if exists('g:loaded_wildignorecase')
    finish
endif
let g:loaded_wildignorecase = 1

function! s:CmdlineTab() abort
    if getcmdtype() ==# ':'
        let token = matchstr(getcmdline()[:getcmdpos()-1], '[^ /]\+$')
        let &wildignorecase = token !~# '[A-Z]'
    endif
    return "\<Tab>"
endfunction

cnoremap <expr> <Tab> <SID>CmdlineTab()

augroup wildignorecase
    autocmd!
    autocmd CmdlineEnter * let g:wic_tmp = &wildignorecase
    autocmd CmdlineLeave * if exists('g:wic_tmp') | let &wildignorecase = g:wic_tmp | unlet g:wic_tmp | endif
augroup END
