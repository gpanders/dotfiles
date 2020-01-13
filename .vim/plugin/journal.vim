" Command to easily open journal
if exists('g:loaded_journal') || empty($JOURNAL_FILE)
    finish
endif
let g:loaded_journal = 1

command! -nargs=0 Journal tabedit $JOURNAL_FILE
