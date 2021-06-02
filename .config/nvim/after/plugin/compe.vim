if !get(g:, 'loaded_compe')
    finish
endif

lua <<EOF
require('compe').setup {
    source = {
        path = true,
        buffer = true,
        nvim_lsp = true,
        nvim_lua = true
    }
}

vim.opt.completeopt = { 'menuone', 'noselect' }
vim.opt.shortmess:append('c')
EOF

inoremap <silent> <expr> <CR> compe#confirm('<CR>')
