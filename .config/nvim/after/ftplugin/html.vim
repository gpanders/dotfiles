" html ftplugin is also sourced for Markdown, so the following should only
" happen on actual HTML files
if &filetype ==# 'html'
    if executable('tidy')
        let &l:formatprg = 'tidy --quiet yes --show-body-only auto --show-info no --show-warnings no --indent auto --tidy-mark no --wrap 0'
        let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<'
    endif
endif
