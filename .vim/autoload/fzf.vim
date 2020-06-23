function! fzf#tags(bang, query, mod) abort
    if !exists('*fzf#run')
        echoerr 'FZF installation not found'
        return
    endif

    let query = !empty(a:query) ? s:prepare_query(a:query) : '.'
    let options = '--no-color --select-1 --no-multi --cycle --nth=1'
    if !empty(a:query)
        let options .= ' --query=' . matchstr(a:query, '^/\zs[^/]\+\ze/\?')
    endif

    call fzf#run(fzf#wrap('Tag', {
                \ 'source': s:tags(query),
                \ 'sink': {t -> s:tag(t, a:mod)},
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

function! s:tags(query) abort
    let query = a:query
    if query =~# '^\\c'
        let ignorecase = 1
        let query = tolower(query[2:])
    else
        let ignorecase = 0
    endif
    let cmd = 'awk -F''\t'''
    let cmd .= ' -v query=''' . query . ''''
    let cmd .= ' -v ic=' . ignorecase
    let cmd .= ' ''/^!/ {next};'
    let cmd .= ' {if ((ic && tolower($1) !~ query) || (!ic && $1 !~ query)) next};'
    let cmd .= ' {sub("/[^/]+/;\"", ""); gsub("(\\.\\./)*", "", $2); printf "%-40s %-10s %s\n", $1, $4, $2}'''
    let cmd .= ' ' . join(tagfiles())
    return cmd
endfunction

function! s:tag(line, mod) abort
    let [tag, _, fname] = split(a:line)[0:2]
    let idx = index(map(taglist('^' . tag . '$', expand('%')), 'v:val.filename'), fname) + 1
    execute idx . a:mod . 'tag ' . tag
endfunction

function! s:prepare_query(query) abort
    if a:query =~# '^/'
        let query = a:query[1:]
        if query =~# '/$'
            let query = query[:-1]
        endif
        return '\c' . query
    else
        return '^' . a:query . '$'
    endif
endfunction
