" mail filetype configuration
" Author: Greg Anders
" Date: 2018-12-02

setlocal formatoptions+=wa
setlocal wrapmargin=0
setlocal nonumber
setlocal nolist
setlocal spell
let b:undo_ftplugin .= '|setl fo< wm< nu< list< spell<'

let b:highlight_trailing_whitespace = 0
let b:undo_ftplugin .= '|unlet! b:highlight_trailing_whitespace'

" See https://en.wikipedia.org/wiki/Posting_style
if !exists('g:mail_posting_style')
    let g:mail_posting_style = 'top'
endif

augroup mail
    autocmd!
    if !&readonly && &modifiable
        " Place cursor in writing position and start insert mode
        if g:mail_posting_style ==# 'top'
            autocmd BufWinEnter <buffer> exe "normal! }2o\<Esc>k" | startinsert
        elseif g:mail_posting_style ==# 'bottom'
            autocmd BufWinEnter <buffer> exe "normal! G2o\<Esc>" | startinsert
        else
            echohl Error | echom 'Unknown value for g:mail_posting_style' | echohl None
        endif
    endif
augroup END
let b:undo_ftplugin .= '|au! mail * <buffer>'
