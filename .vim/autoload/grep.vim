" Expand characters from :h cmdline-special
let s:expandable = '\v(\\@<!%(\%|##|#\d*|<%(cfile|cword|cWORD|cexpr)>)%(:[p~.htreS])*)'
function! s:expandcmd(cmd) abort
    if exists('*expandcmd')
        return expandcmd(a:cmd)
    else
        return substitute(a:cmd, s:expandable, '\=expand(submatch(1))', 'g')
    endif
endfunction

function! s:grep(l, args, mods) abort
    let args = s:expandcmd(a:args)
    if &grepprg =~# '$\*'
        let grepcmd = substitute(&grepprg, '$\*', args, '')
    else
        let grepcmd = join([&grepprg] + [args], ' ')
    endif

    let F = a:l ? function('setloclist', [0]) : function('setqflist')
    let post = 'doautocmd QuickFixCmdPost ' . (a:l ? 'lgrep' : 'grep')

    silent exe 'doautocmd QuickFixCmdPre' a:l ? 'lgrep' : 'grep'

    if g:async#enabled
        if exists('s:jobid')
            call async#cancel(s:jobid)
            unlet s:jobid
        endif

        " Run the grep command in a shell to enable shell expansion
        let cmd = split(&shell) + split(&shellcmdflag) + [grepcmd]
        let opts = {'buffered': 0, 'exit': {code -> !code && execute(post . '|unlet s:jobid')}}
        let s:jobid = async#run(cmd, {lines -> F([], 'a', {'lines': lines})}, opts)
        call F([], ' ', {'title': grepcmd, 'nr': '$', 'items': []})
    else
        call F([], ' ', {'title': grepcmd, 'nr': '$', 'lines': systemlist(grepcmd)})
        silent exe post
    endif
    exe a:mods a:l ? 'lopen' : 'copen'
endfunction

function! grep#grep(args, mods) abort
    call s:grep(0, a:args, a:mods)
endfunction

function! grep#lgrep(args, mods) abort
    call s:grep(1, a:args, a:mods)
endfunction
