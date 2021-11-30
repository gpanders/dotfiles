let s:actions = ['hover', 'code_action', 'definition', 'formatting', 'references', 'rename', 'signature_help']

function! lsp#complete(arg, line, pos) abort
    if count(a:line[:a:pos], ' ') == 1
        return join(s:actions, "\n")
    end
    return ''
endfunction
