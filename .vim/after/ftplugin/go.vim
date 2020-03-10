if executable('gofmt')
    setlocal formatprg=gofmt
    let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<'
endif

if exists('$GOPATH') && filereadable($GOPATH . '/tags')
    let &l:tags = $GOPATH . '/tags,' . &l:tags
endif
