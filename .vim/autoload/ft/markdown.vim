function! s:openlink(link, action) abort
    if a:link =~# '^https\?://'
        if executable('xdg-open')
            call system('xdg-open ' . a:link)
        elseif executable('open')
            call system('open ' . a:link)
        endif
    elseif a:link =~# '^/' " Absolute path
        exec a:action . ' ' . a:link
    elseif !empty(a:link) " Relative path
        exec a:action . ' ' . simplify(expand('%:h') . '/' . a:link)
    endif
endfunction

function! s:openreflink(name, action) abort
    let l = search('^\s*\[' . a:name . '\]\s*:\s\+', 'nw')
    if l
        let link = matchlist(getline(l), '^\s*\[' . a:name . '\]\s*:\s\+\(\S\+\)')[1]
        call s:openlink(link, a:action)
    endif
endfunction

function! ft#markdown#open(action) abort
    let line = getline('.')
    let [_, col] = searchpos('\[[^]]\+\]\(([^)]\+)\|\[[^]]\+]\|\[]\|[^[(]\=\)', 'bcn', line('.'))
    if col == 0
        return
    endif

    let matches = matchlist(line, '\(\[[^][]\+\]\)\(([^)]\+)\|\[[^]]\+]\|\[]\|[^[(]\=\)', col-1)
    if matches[2] =~# '([^)]\+)'
        " Normal link
        let link = matchstr(matches[2], '(\zs\([^)]\+\)\ze)')
        call s:openlink(link, a:action)
    elseif matches[2] =~# '\[[^]]\+]'
        " Reference link
        let ref = matchstr(matches[2], '\[\zs\([^]]\+\)\ze]')
        call s:openreflink(ref, a:action)
    else
        let ref = matchstr(matches[1], '\[\zs\([^]]\+\)\ze]')
        call s:openreflink(ref, a:action)
    endif
endfunction

function! ft#markdown#toc()
    let fp = &l:formatprg
    let &l:formatprg .= ' --toc'
    let view = winsaveview()
    normal! gggqG
    call winrestview(view)
    let &l:formatprg = fp
endfunction

function! ft#markdown#setopt(opt, bang)
    if a:bang
        if &l:formatprg =~# '--' . a:opt
            let &l:formatprg = substitute(&l:formatprg, ' --' . a:opt, '', '')
        endif
    else
        if &l:formatprg !~# '--' . a:opt
            let &l:formatprg .= ' --' . a:opt
        endif
    endif
    let view = winsaveview()
    normal! gggqG
    call winrestview(view)
endfunction
