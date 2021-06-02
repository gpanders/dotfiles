let s:awk = 'awk -F, '
            \ . '{for (i = 1; i < NF; i++) { '
            \ .   'len = length($(i)); '
            \ .   'if ( len > maxlen ) maxlen = len '
            \ . '}}'
            \ . 'END { print maxlen }'''

function! ft#csv#format() abort
    if mode() ==# 'i' || mode() ==# 'R'
        return 0
    endif

    let start = v:lnum
    let end = v:lnum + v:count - 1

    let ts = systemlist(s:awk, join(getline(start, end), "\n"))[0]
    let &l:tabstop = ts + 1

    for lnum in range(start, end)
        let line = getline(lnum)
        if line =~# '\t'
            let pat = '\t'
            let str = ','
        else
            let pat = ','
            let str = '\t'
        endif
        call setline(lnum, substitute(line, pat, str, 'g'))
    endfor
endfunction
