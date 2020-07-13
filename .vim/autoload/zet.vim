function! zet#complete(findstart, base) abort
    let line = getline('.')
    if line =~# '^tags:'
        return s:compltag(line, a:findstart, a:base)
    else
        return s:compllink(line, a:findstart, a:base)
    endif
endfunction

function! s:zettels(base) abort
    let zettels = glob($ZETTEL_DIR . '/*.md', 0, 1)
    call map(zettels, 'fnamemodify(v:val, '':t'')')
    call filter(zettels, 'v:val =~# ''^\d\{14}'' && v:val =~? a:base')
    return zettels
endfunction

function! s:compltag(line, findstart, base) abort
    if a:findstart
        let start = col('.') - 1
        while start > 0 && a:line[start - 1] !~# '[, ]'
            let start -= 1
        endwhile
        if start == 0
            return -2
        endif
        return start
    else
        let zettels = s:zettels('')
        let alltags = []
        for zet in zettels
            let contents = readfile($ZETTEL_DIR . '/' . zet, 0, 5)
            let tags = split(matchstr(contents, '^tags:\s\+.\+$'), '^tags:\s\+')
            if empty(tags)
                continue
            endif

            call extend(alltags, split(tags[0], ', \?'))
        endfor

        call uniq(sort(alltags))

        let matches = []
        for tag in alltags
            if tag =~# a:base
                call add(matches, tag)
            endif
        endfor
        return matches
    endif
endfunction

function! s:compllink(line, findstart, base) abort
    if a:findstart
        let start = col('.') - 1
        while start > 0 && a:line[start - 2:start - 1] !=# '[['
            let start -= 1
        endwhile
        if start == 0
            return -2
        endif
        return start
    else
        let zettels = s:zettels(a:base)
        let matches = []
        for zet in zettels
            let contents = readfile($ZETTEL_DIR . '/' . zet, 0, 5)
            let title = split(matchstr(contents, '^title:\s\+.\+$'), '^title:\s\+')
            if !empty(title)
                let word = matchstr(zet, '^\d\+')
                call add(matches, {'word': word, 'menu': title[0]})
            endif
        endfor
        return matches
    endif
endfunction
