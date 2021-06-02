function! s:cb(l, title, mods, data) abort
    let F = a:l ? function('setloclist', [0]) : function('setqflist')
    call F([], ' ', {'title': a:title, 'efm': &grepformat, 'nr': '$', 'lines': a:data})
    exe a:mods a:l ? 'lopen' : 'copen'
    echo
endfunction

function! s:grep(l, args, mods) abort
    let args = expandcmd(a:args)
    if &grepprg =~# '$\*'
        let grepcmd = substitute(&grepprg, '$\*', args, '')
    else
        let grepcmd = join([&grepprg] + [args], ' ')
    endif

    let cmdpost = 'doautocmd QuickFixCmdPost ' . (a:l ? 'lgrep' : 'grep')
    silent exe 'doautocmd QuickFixCmdPre' a:l ? 'lgrep' : 'grep'

    let opts = {'stdout_buffered': v:true}
    let opts.on_stdout = {j,d,e -> s:cb(a:l, grepcmd, a:mods, d)}
    let opts.on_exit = {... -> execute(cmdpost)}

    call jobstart(grepcmd, opts)
    echo grepcmd
endfunction

function! grep#grep(args, mods) abort
    call s:grep(0, a:args, a:mods)
endfunction

function! grep#lgrep(args, mods) abort
    call s:grep(1, a:args, a:mods)
endfunction
