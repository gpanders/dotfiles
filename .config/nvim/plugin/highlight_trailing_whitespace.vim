if exists('g:loaded_highlight_trailing_whitespace')
    finish
endif
let g:loaded_highlight_trailing_whitespace = 1

let s:blacklist = ['markdown', 'text', 'mail', 'gitsendemail']

function! s:highlight_trailing_whitespace() abort
    let modifiable = !&readonly && &modifiable
    if modifiable && &filetype !=# '' && index(s:blacklist, &filetype) == -1
        syn match TrailingWhitespace /\\\@<!\s\+\%#\@<!$/ containedin=ALL
    endif
endfunction

augroup highlight_trailing_whitespace
    autocmd!
    autocmd Syntax * call s:highlight_trailing_whitespace()
augroup END
