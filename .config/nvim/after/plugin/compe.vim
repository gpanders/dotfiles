if !get(g:, 'loaded_compe')
  finish
endif

lua <<EOF
local compe = require('compe')

function setupcompe()
    local ft = vim.opt.filetype:get()
    local buffer = true
    for _, v in ipairs({"mail", "markdown", "text", "rst", "gitcommit"}) do
        if ft == v then
            buffer = false
            break
        end
    end

    compe.setup({
        source = {
            path = true,
            buffer = buffer,
            nvim_lsp = true,
            nvim_lua = true
        },
    }, 0)
end

vim.opt.completeopt = { 'menuone', 'noselect' }
vim.opt.shortmess:append('c')
EOF

inoremap <silent> <expr> <CR> compe#confirm('<CR>')

augroup my_compe
  autocmd!
  autocmd FileType * lua setupcompe()
augroup END
