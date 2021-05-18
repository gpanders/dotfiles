if executable('gofmt')
    setlocal formatprg=gofmt
    augroup ftplugin_go
        autocmd! * <buffer>
        autocmd BufWritePre <buffer> call ft#go#fmt()
    augroup END
    let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<|au! ftplugin_go * <buffer>'
endif

if exists('$GOPATH') && filereadable($GOPATH . '/tags')
    let &l:tags = $GOPATH . '/tags,' . &l:tags
endif
