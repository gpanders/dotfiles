" Source: https://gist.github.com/romainl/eae0a260ab9c135390c30cd370c20cd7
function! scratch#open(cmd) abort
    for win in filter(range(1, winnr('$')), 'getwinvar(v:val, "scratch")')
        execute win . 'windo close'
    endfor
    if a:cmd =~ '^!'
        let cmd = a:cmd =~' %' ? substitute(a:cmd, ' %', ' ' . expand('%:p'), '') : a:cmd
        let output = system(matchstr(cmd, '^!\zs.*'))
    else
        let output = execute(a:cmd)
    endif
    vnew
    let w:scratch = 1
    setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile
    call setline(1, split(output, "\n"))
endfunction
