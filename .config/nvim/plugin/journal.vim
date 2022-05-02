" Command to easily open journal
if exists('g:loaded_journal') || empty($JOURNAL_FILE)
    finish
endif
let g:loaded_journal = 1

function! s:entry()
    let date = strftime('%a, %d %b, %Y')
    if !search(date, '')
        let lines = [date, repeat('=', len(date)), '', '', '']
        call append(0, lines)
        call cursor(len(lines) - 1, 1)
    endif
endfunction

augroup journal
    autocmd!
    autocmd BufRead $JOURNAL_FILE call s:entry()
    autocmd BufWinEnter $JOURNAL_FILE setlocal foldlevel=0
    autocmd BufWinEnter $JOURNAL_FILE let w:view = winsaveview() |
                \ silent! exec 'normal! 1Gzo' |
                \ call winrestview(w:view) |
                \ unlet w:view
augroup END

command! -nargs=0 Journal edit $JOURNAL_FILE
