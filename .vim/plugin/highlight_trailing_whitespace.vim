if exists('g:loaded_highlight_trailing_whitespace')
    finish
endif
let g:loaded_highlight_trailing_whitespace = 1

let s:blacklist = ['markdown', 'text', 'mail']

function! s:highlight_trailing_whitespace() abort
    let ft = &filetype
    let modifiable = !&readonly && &modifiable
    if modifiable && ft !=# '' && index(s:blacklist, ft) == -1
        syn match TrailingWhitespace /\\\@<!\s\+\%#\@<!$/ containedin=ALL
    endif
endfunction

augroup highlight_trailing_whitespace
    autocmd!
    autocmd Syntax * call s:highlight_trailing_whitespace()
augroup END
