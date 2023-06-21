" Source: https://gist.github.com/romainl/eae0a260ab9c135390c30cd370c20cd7
function! scratch#open(cmd, mods) abort
    for win in filter(range(1, winnr('$')), 'getwinvar(v:val, "scratch")')
        execute win . 'windo close'
    endfor
    if a:cmd =~# '^!'
        let cmd = a:cmd =~# ' %' ? substitute(a:cmd, ' %', ' ' . expand('%:p'), '') : a:cmd
        let output = systemlist(matchstr(cmd, '^!\zs.*'))
    elseif a:cmd =~# '^@'
        let output = getreg(a:cmd[1], 1, 1)
    else
        let output = split(execute(a:cmd), "\n")
    endif
    execute a:mods . ' new'
    let w:scratch = 1
    call setline(1, output)
    nnoremap <buffer> q <C-W>q
    setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nomodifiable
    call s:highlight(a:cmd)

    augroup scratch
      autocmd BufEnter <buffer> ++nested if winnr('$') < 2 | q | endif
    augroup END
endfunction

" Highlight scratch buffer to match native command output for certain commands
function! s:highlight(cmd) abort
    let firstline = getline(1)
    if firstline =~# '^--- Registers ---\|Type Name Content$'
        call matchaddpos('Title', [1])
        call matchadd('SpecialKey', '\^\S\|<[^>]\{2}>')
    elseif firstline =~# '^\%(mark\|change\| jump\) line  col \%(file/\)\?text$'
        call matchaddpos('Title', [1])
    elseif firstline =~# '^\w\+\s\+xxx \%(links\|cleared\|cterm\|gui\|term\)'
        syntax match Keyword /\v%(c?term%([fb]g)?|gui%([fb]g)?)\=/
        syntax keyword Keyword links to
        for linenr in range(1, line('$'))
            let line = getline(linenr)
            let syn = matchstr(line, '^\w\+')
            let col = match(line, '\<xxx\>')
            call matchaddpos(syn, [[linenr, col + 1, 3]])
        endfor
    elseif search('\v^(E[0-9]+:|line\s+[0-9]+:)', 'cnW')
        call matchadd('ErrorMsg', '^E[0-9]\+: .\+$')
        call matchadd('LineNr', '^line\s\+[0-9]\+:$')
    elseif a:cmd =~# '^dig'
        call matchadd('SpecialKey', '[A-Za-z0-9[:graph:]]\{2}\s\+\zs\S\+\ze\s\+\d\+')
    endif
endfunction
