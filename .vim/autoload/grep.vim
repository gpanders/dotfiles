" Expand characters from :h cmdline-special
let s:expandable = '\v(\\@<!%(\%|##|#\d*|<%(cfile|cword|cWORD|cexpr)>)%(:[p~.htreS])*)'
function! s:expandcmd(cmd) abort
    if exists('*expandcmd')
        return expandcmd(a:cmd)
    else
        return substitute(a:cmd, s:expandable, '\=expand(submatch(1))', 'g')
    endif
endfunction

function! grep#grep(l, args) abort
    let args = s:expandcmd(a:args)
    if &grepprg =~# '$\*'
        let grepcmd = substitute(&grepprg, '$\*', args, '')
    else
        let grepcmd = join([&grepprg] + [args], ' ')
    endif

    let F = a:l ? function('setloclist', [0]) : function('setqflist')
    let completed = 'doautocmd QuickFixCmdPost ' . (a:l ? 'lgrep' : 'grep')

    silent exe 'doautocmd QuickFixCmdPre' a:l ? 'lgrep' : 'grep'

    if g:async#enabled
        " Run the grep command in a shell to enable shell expansion
        let cmd = split(&shell) + split(&shellcmdflag) + [grepcmd]
        call async#run(cmd, {lines -> F([], 'a', {'lines': lines})}, {'buffered': 0, 'completed': completed})
        call F([], 'r', {'title': grepcmd, 'items': []})
    else
        call F([], 'r', {'title': grepcmd, 'lines': systemlist(grepcmd)})
        silent exe completed
    endif
    exe 'botright' a:l ? 'lopen' : 'copen'
endfunction
