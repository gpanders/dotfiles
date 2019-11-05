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
    setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile
    call setline(1, output)
endfunction
