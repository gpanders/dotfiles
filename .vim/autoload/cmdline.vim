let s:expandable = '\v^([%#$]|<(cfile|cword|cWORD|cexpr)>)'

function! s:expand(cmdline) abort
    let tokens = split(a:cmdline)
    let tokens[-1] = expand(tokens[-1])
    return join(tokens)
endfunction

function! cmdline#expand(trigger) abort
    let cmdline = getcmdline()[0:getcmdpos()-1]
    if getcmdtype() ==# ':' && split(cmdline)[-1] =~# s:expandable
        return "\<C-\>e'" . s:expand(cmdline) . "'\<CR>"
    else
        return a:trigger
    endif
endfunction
