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

function! s:tags(split, line) abort
    let [tag, _, filename] = split(a:line)[0:2]
    let index = index(map(taglist('^' . tag . '$'), 'v:val.filename'), filename) + 1
    execute index . (a:split ? 's' : '') . 'tag ' . tag
endfunction

function! s:fzy(cmd, action, title) abort
    let file = tempname()
    let winid = win_getid()
    let cmd = split(&shell) + split(&shellcmdflag) + [a:cmd . ' | fzy > ' . file]
    let F = function('s:completed', [winid, file, a:action])
    botright 10 new
    if exists('*termopen')
        call termopen(cmd, {'on_exit': F})
    elseif exists('*term_start')
        call term_start(cmd, {'exit_cb': F, 'curwin': 1, 'term_kill': 'quit'})
    else
        echohl ErrorMsg
        echom 'No terminal API available'
        echohl None
        return F('')
    end
    exe 'file ' . a:title
    startinsert
endfunction

function! fzy#files(...)
    let opts = a:0 ? a:1 : {}
    let split = get(opts, 'split')
    let cmd = split ? 'sp' : 'e'
    call s:fzy(get(b:, 'fzy_find_files_cmd', g:fzy_find_files_cmd), cmd, 'Files')
endfunction

function! fzy#tags(...)
    let opts = a:0 ? a:1 : {}
    let split = get(opts, 'split')
    let tags = map(taglist('.'), {_, v -> printf('%-40s %-12s %s', v.name, v.kind, fnamemodify(v.filename, ':.'))})
    let file = tempname()
    call writefile(tags, file)
    call s:fzy('cat ' . file, {t -> s:tags(split, t) && delete(file)}, 'Tags')
endfunction

function! fzy#buffers(...)
    let opts = a:0 ? a:1 : {}
    let split = get(opts, 'split')
    let buffers = map(getbufinfo({'buflisted': 1}), {_, v -> printf('%-3d %s', v.bufnr, fnamemodify(v.name, ':.'))})
    let file = tempname()
    call writefile(buffers, file)
    call s:fzy('cat ' . file, {b -> execute((split ? 's' : '') . 'b' . split(b)[0]) && delete(file)}, 'Buffers')
endfunction
