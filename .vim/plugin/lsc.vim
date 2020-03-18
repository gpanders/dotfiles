let g:lsc_auto_map = {'defaults': v:true, 'WorkspaceSymbol': '', 'FindCodeActions': 'gA', 'NextReference': ']r', 'PreviousReference': '[r'}
let g:lsc_enable_diagnostics = v:false
let g:lsc_enable_popup_syntax = v:false

" Configure LSP servers
let g:lsc_server_commands = {
            \ 'python': {'command': 'pyls', 'suppress_stderr': v:true},
            \ 'c': {'command': '/usr/local/opt/llvm/bin/clangd', 'suppress_stderr': v:true},
            \ 'cpp': {'command': '/usr/local/opt/llvm/bin/clangd', 'suppress_stderr': v:true},
            \ 'go': {'command': 'gopls serve', 'log_level': -1, 'suppress_stderr': v:true},
            \ }
