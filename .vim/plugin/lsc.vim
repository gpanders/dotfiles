let g:lsc_auto_map = {
            \   'defaults': v:true,
            \   'FindReferences': '<C-_>r',
            \   'NextReference': ']r',
            \   'PreviousReference': '[r',
            \   'FindImplementations': '<C-_>I',
            \   'DocumentSymbol': '<C-_>o',
            \   'WorkspaceSymbol': '<C-_>S',
            \   'FindCodeActions': '<C-_>a',
            \   'Rename': '<C-_>R',
            \   'SignatureHelp': '<C-_>m',
            \ }

let g:lsc_enable_diagnostics = v:false

" Configure LSP servers
let g:lsc_server_commands = {}

if executable('pyls')
    let g:lsc_server_commands.python = {'command': 'pyls', 'suppress_stderr': v:true}
endif

if executable('clangd')
    let g:lsc_server_commands.c = {'command': 'clangd', 'suppress_stderr': v:true}
    let g:lsc_server_commands.cpp = g:lsc_server_commands.c
endif

if executable('gopls')
    let g:lsc_server_commands.go = {'command': 'gopls serve', 'log_level': -1, 'suppress_stderr': v:true}
endif

if executable('rls')
    let g:lsc_server_commands.rust = {'command': 'rls', 'suppress_stderr': v:true}
endif
