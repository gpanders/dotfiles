" Use FZF to find help tags
" Author: Greg Anders
" Date: 2019-05-07

function! fzf#helptags(bang)
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

function! fzf#tags(bang, query, mod)
    if !exists('*fzf#run')
        echohl ErrorMsg
        echom 'FZF installation not found'
        echohl None
        return
    endif

    let tags = map(taglist('.'), {_, v -> printf('%-40s %-10s %s', v.name, v.kind, v.filename)})
    let options = '--select-1 --no-multi'
    if !empty(a:query)
        let options .= ' --query=' . a:query
    endif

    call fzf#run(fzf#wrap('Tags', {
                \ 'source': tags,
                \ 'sink': {t -> execute(a:mod . 'tag ' . split(t)[0])},
                \ 'options': options,
                \ }, a:bang))
endfunction
