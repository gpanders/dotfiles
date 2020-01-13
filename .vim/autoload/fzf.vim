function! fzf#helptags(bang) abort
    if !exists('*fzf#run')
        echohl ErrorMsg
        echom 'FZF installation not found'
        echohl None
        return
    endif

    let tags = uniq(sort(globpath(&runtimepath, 'doc/tags', 0, 1)))
    call fzf#run(fzf#wrap('Helptags', {
                \ 'source': 'grep -ho "^\S\+" ' . join(tags),
                \ 'sink': 'help',
                \ 'options': '--no-multi',
                \ }, a:bang))
endfunction

function! fzf#tags(bang, query, mod) abort
    if !exists('*fzf#run')
        echohl ErrorMsg
        echom 'FZF installation not found'
        echohl None
        return
    endif

    let tags = map(taglist('.'), {_, v -> printf('%-40s %-10s %s', v.name, v.kind, fnamemodify(v.filename, ':.'))})
    let options = '--select-1 --no-multi --cycle'
    if !empty(a:query)
        let options .= ' --query=' . a:query
    endif

    call fzf#run(fzf#wrap('Tag', {
                \ 'source': tags,
                \ 'sink': {t -> s:tags(t, a:mod)},
                \ 'options': options,
                \ }, a:bang))
endfunction

function! s:tags(line, mod) abort
    let [tag, _, filename] = split(a:line)[0:2]
    let index = index(map(taglist('^' . tag . '$'), 'v:val.filename'), filename) + 1
    execute index . a:mod . 'tag ' . tag
endfunction
