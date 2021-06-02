let s:placeholder = '{%}'

" Map filetypes to extensions for files that don't have extensions but do have
" a known filetype. If an extension and its filetype are the same (e.g. 'c')
" then it doesn't need an entry
let s:extmap = {'python': 'py', 'ruby': 'rb'}

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
    if ext ==# ''
        let ext = get(s:extmap, &filetype, &filetype)
    endif

    if filereadable(g:snippets_dir . '/' . token . '.' . ext)
        let file = g:snippets_dir . '/' . token . '.' . ext
    elseif filereadable(g:snippets_dir . '/' . token . '.' . &filetype)
        let file = g:snippets_dir . '/' . token . '.' . &filetype
    else
        return "\<Tab>"
    endif

    delete _
    let snippet = readfile(file)
    call append(lnum - 1, snippet)
    normal! '[

    let [l, c] = searchpos(s:placeholder, '', lnum + len(snippet) - 1)
    if l
        call setline(l, substitute(getline(l), s:placeholder, '', ''))
    endif

    " Must return an empty string so that no extra characters are inserted
    return ''
endfunction
