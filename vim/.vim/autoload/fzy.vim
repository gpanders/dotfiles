function! s:completed(winid, filename, action, msg) abort
    bdelete!
    call win_gotoid(a:winid)
    if filereadable(a:filename)
        try
            let selection = readfile(a:filename)[0]
            if type(a:action) == type('')
                exe a:action . ' ' . selection
            elseif type(a:action) == type(function('tr'))
                call a:action(selection)
            endif
        catch /E684/
            " Ignore error if no selection
        endtry
    endif
    call delete(a:filename)
endfunction

function! s:fzy(cmd, action, title)
    let file = tempname()
    call async#run(a:cmd . ' | fzy > ' . file, v:null, {
                \ 'completed': function('s:completed', [win_getid(), file, a:action]),
                \ 'term': 1,
                \ 'title': a:title,
                \ 'height': 10,
                \ })
endfunction

function! fzy#files()
    call s:fzy(g:fzy_find_files_cmd, 'e', 'files')
endfunction

function! fzy#tags()
    let tags = map(taglist('.'), {_, v -> printf('%-40s %-10s %s', v.name, v.kind, v.filename)})
    let file = tempname()
    call writefile(tags, file)
    call s:fzy('cat ' . file, {t -> execute('tag ' . split(t)[0])}, 'tags')
endfunction


