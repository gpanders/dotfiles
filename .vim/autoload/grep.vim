" Expand characters from :h cmdline-special
let s:expandable = '\v(^| )(%(\%|#\d*|##|<%(cfile|cword|cWORD|cexpr)>)%(:[p~.htreS])*)%( |$)'
function! s:expandcmd(cmd) abort
    if exists('*expandcmd')
        return expandcmd(a:cmd)
    else
        return substitute(a:cmd, s:expandable, '\=submatch(1) . expand(submatch(2) . '':S'')', '')
    endif
endfunction

function! grep#grep(l, ...) abort
    " Separate options from arguments
    let opts = []
    let args = []
    for arg in a:000
        call add(arg =~# '^-' ? opts : args, arg)
    endfor

    " Put flags first, followed by first argument without expanding, followed
    " by the rest of the arguments expanded
    let args = join(opts + [args[0]] + [s:expandcmd(join(args[1:], ' '))], ' ')

    if stridx(&grepprg, '$*') != -1
        let grepcmd = substitute(&grepprg, '$\*', args, 'g')
    else
        let grepcmd = &grepprg . ' ' . args
    endif

    " Run the grep command in a shell to enable shell expansion
    let cmd = split(&shell) + split(&shellcmdflag) + [grepcmd]
    let F = a:l ? function('setloclist', [0]) : function('setqflist')
    silent exe 'doautocmd QuickFixCmdPre' a:l ? 'lgrep' : 'grep'
    let completed = 'doautocmd QuickFixCmdPost ' . (a:l ? 'lgrep' : 'grep')

    if g:async#enabled
        call async#run(cmd, {lines -> F([], 'a', {'lines': lines})}, {'buffered': 0, 'completed': completed})
        call F([], 'r', {'title': grepcmd, 'items': []})
    else
        call F([], 'r', {'title': grepcmd, 'lines': systemlist(grepcmd)})
        silent exe completed
    endif
    exe 'botright' a:l ? 'lopen' : 'copen'
endfunction
