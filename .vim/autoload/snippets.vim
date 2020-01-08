function! snippets#expand() abort
    let lnum = line('.')
    let col = col('.')
    let line = getline('.')
    let curline = line[0:col-1]
    if curline =~# '^\s*$'
        return "\<Tab>"
    endif

    let token = split(curline[0:col-1])[-1]
    let ext = expand('%:e')
    if filereadable(g:snippets_dir . '/' . token . '.' . ext)
        let file = g:snippets_dir . '/' . token . '.' . ext
    elseif filereadable(g:snippets_dir . '/' . token . '.' . &filetype)
        let file = g:snippets_dir . '/' . token . '.' . &filetype
    else
        return "\<Tab>"
    endif

    delete _
    execute lnum-1 . 'read ' . file
    normal! =']
    return ''
endfunction
