let s:servers = [#{filetype: ['c', 'cpp'], path: 'clangd', args: ['--background-index']}]

call LspAddServer(s:servers)
