function! s:completed(winid, filename, action, ...) abort
    bdelete!
    call win_gotoid(a:winid)
    if filereadable(a:filename)
        let lines = readfile(a:filename)
        if !empty(lines)
            if type(a:action) ==# type('')
                exe a:action . ' ' . lines[0]
            elseif type(a:action) ==# type({->0})
                call a:action(lines[0])
            endif
        endif
        call delete(a:filename)
    endif
endfunction

function! s:fzy(cmd, action, title)
    let file = tempname()
    let winid = win_getid()
    let cmd = split(&shell) + split(&shellcmdflag) + [a:cmd . ' | fzy > ' . file]
    let F = function('s:completed', [winid, file, a:action])
    botright 10 new
    if has('nvim')
        call termopen(cmd, {'on_exit': F})
    else
        call term_start(cmd, {'exit_cb': F, 'curwin': 1, 'term_kill': 'quit'})
    endif
    exe 'file ' . a:title
    startinsert
endfunction

function! fzy#files()
    call s:fzy(get(b:, 'fzy_find_files_cmd', g:fzy_find_files_cmd), 'e', 'files')
endfunction

function! fzy#tags()
    let tags = map(taglist('.'), {_, v -> printf('%-40s %-10s %s', v.name, v.kind, v.filename)})
    let file = tempname()
    call writefile(tags, file)
    call s:fzy('cat ' . file, {t -> execute('tag ' . split(t)[0])}, 'tags')
endfunction
