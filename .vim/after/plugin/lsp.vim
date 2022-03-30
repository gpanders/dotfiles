vim9script

var servers = [
    {filetype: ['c', 'cpp'], path: 'clangd', args: ['--background-index']},
]

autocmd FileType c,cpp ++once {
    packadd lsp
    call LspAddServer(servers)
    doautocmd <nomodeline> LSPAutoCmds BufRead
}
