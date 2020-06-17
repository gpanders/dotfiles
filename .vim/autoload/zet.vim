function! zet#complete(findstart, base)
    if a:findstart
        let line = getline('.')
        let start = col('.') - 1
        while start > 0 && line[start - 2:start - 1] !=# '[['
            let start -= 1
        endwhile
        return start
    else
        let zettels = glob($ZETTEL_DIR . '/*.md', 0, 1)
        call map(zettels, 'fnamemodify(v:val, '':t'')')
        call filter(zettels, 'v:val =~? a:base')

        let matches = []
        for zet in zettels
            let word = matchstr(zet, '^\d\+')
            let contents = readfile($ZETTEL_DIR . '/' . zet, 0, 5)
            let title = split(matchstr(contents, '^title: .\+$'), '^title: ')
            call add(matches, {'word': word, 'menu': title[0]})
        endfor
        return matches
    endif
endfunction
