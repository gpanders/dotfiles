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
        let jobid = termopen(cmd, {'on_exit': F})
        augroup fzy
            exec 'autocmd TermLeave <buffer> call jobstop(' . jobid . ')'
        augroup END
    elseif exists('*term_start')
        call term_start(cmd, {'exit_cb': F, 'curwin': 1})
    else
        echohl ErrorMsg
        echom 'No terminal API available'
        echohl None
        return F('')
    end
    exe 'file ' . a:title
    startinsert
endfunction

function! fzy#files(...) abort
    let opts = a:0 ? a:1 : {}
    let split = get(opts, 'split')
    let cmd = split ? 'sp' : 'e'
    call s:fzy(get(b:, 'fzy_find_files_cmd', g:fzy_find_files_cmd), cmd, 'Files')
endfunction

function! fzy#tags(...) abort
    if empty(tagfiles())
        echohl ErrorMsg
        echom 'No tag files'
        echohl None
        return
    endif

    let opts = a:0 ? a:1 : {}
    let split = get(opts, 'split')

    let cmd = 'awk -F''\t'' ''/^!/{next};{sub("/[^/]+/;\"","");gsub("([.]{2}/)+","",$2);printf "%-40s %-10s %s\n", $1, $4, $2}'' ' . join(tagfiles())
    call s:fzy(cmd, {t -> s:tags(split, t)}, 'Tags')
endfunction

function! fzy#buffers(...) abort
    let opts = a:0 ? a:1 : {}
    let split = get(opts, 'split')
    let buffers = map(getbufinfo({'buflisted': 1}), {_, v -> printf('%-3d %s', v.bufnr, fnamemodify(v.name, ':.'))})
    let file = tempname()
    call writefile(buffers, file)
    call s:fzy('cat ' . file, {b -> execute((split ? 's' : '') . 'b' . split(b)[0]) && delete(file)}, 'Buffers')
endfunction
