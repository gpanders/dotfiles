function! fzf#tags(bang, query, mod) abort
    if !exists('*fzf#run')
        echoerr 'FZF installation not found'
        return
    endif

    let query = !empty(a:query) ? a:query : '.'
    if query =~# '^/'
        let query = query[1:]
        if query =~# '/$'
            let query = query[:-1]
        endif
    else
        let query = '\c^' . query
    endif

    let tags = map(taglist(query), {_, v -> printf('%-40s %-10s %s', v.name, v.kind, fnamemodify(v.filename, ':.'))})
    let options = '--select-1 --no-multi --cycle --nth=1'
    if !empty(a:query)
        let options .= ' --query=' . matchstr(a:query, '^/\zs[^/]\+\ze/\?')
    endif

    call fzf#run(fzf#wrap('Tag', {
                \ 'source': tags,
                \ 'sink': {t -> s:tags(t, a:mod)},
                \ 'options': options,
                \ }, a:bang))
endfunction

function! fzf#buffers(bang, mod) abort
    if !exists('*fzf#run')
        echoerr 'FZF installation not found'
        return
    endif

    let buffers = map(getbufinfo({'buflisted': 1}), {_, v -> printf('%-3d %s', v.bufnr, fnamemodify(v.name, ':.'))})

    call fzf#run(fzf#wrap('Buffers', {
                \ 'source': buffers,
                \ 'sink': {b -> execute(a:mod . 'b' . split(b)[0])},
                \ 'options': '--layout=default --no-multi --tiebreak=end,length',
                \ }, a:bang))
endfunction

function! s:tags(line, mod) abort
    let [tag, _, filename] = split(a:line)[0:2]
    let index = index(map(taglist('^' . tag . '$'), 'v:val.filename'), filename) + 1
    execute index . a:mod . 'tag ' . tag
endfunction
