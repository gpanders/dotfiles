if exists('g:loaded_wildignorecase')
    finish
endif
let g:loaded_wildignorecase = 1

set nofileignorecase
set wildcharm=<Tab>

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
    autocmd CmdlineEnter : let s:wic = &wildignorecase
    autocmd CmdlineLeave : if exists('s:wic') | let &wildignorecase = s:wic | endif
augroup END
