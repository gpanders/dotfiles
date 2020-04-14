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
    autocmd CmdlineEnter * let b:wic = &wildignorecase
    autocmd CmdlineLeave * let &wildignorecase = b:wic | unlet! b:wic
augroup END
