function! s:shellsplit(str)
    return map(split(a:str, '\%(^\%("[^"]*"\|''[^'']*''\|[^"'']\)*\)\@<= '), {_, v -> substitute(v, '^["'']\|["'']$', '', 'g')})
endfunction

function! grep#grep(l, args) abort
    " Separate options from arguments
    let opts = []
    let args = []
    for arg in s:shellsplit(a:args)
        call add(arg =~# '^-' ? opts : args, arg)
    endfor

    " Expand filename arguments and shellescape all args
    let args = map([args[0]] + map(args[1:], 'expand(v:val)'), 'shellescape(v:val)')

    if &grepprg =~# '$\*'
        let grepcmd = substitute(&grepprg, '$\*', join(opts + args, ' '), '')
    else
        let grepcmd = join([&grepprg] + opts + args, ' ')
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
