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
    autocmd CmdlineEnter * let wic_save = &wildignorecase
    autocmd CmdlineLeave * if exists('wic_save') | let &wildignorecase = wic_save | unlet wic_save | endif
augroup END
