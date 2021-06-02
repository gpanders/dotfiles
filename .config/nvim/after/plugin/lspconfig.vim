if !get(g:, 'lspconfig')
    finish
endif

lua << EOF
local lsp = require('lspconfig')

local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    local opts = { noremap = true, silent = true }

    buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', 'gR', '<Cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', 'gh', '<Cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)

    vim.cmd 'doautocmd User LspAttached'

    -- Disable Neomake for LSP buffers
    vim.cmd [[
        silent call neomake#cmd#disable(b:)
        silent call neomake#cmd#clean(1)
    ]]
end

local servers = {
    rust_analyzer = {},
    clangd = {},
    gopls = {},
    pyls = {
        pyls = {
            configurationSources = { 'flake8' },
        },
    },
}

for server, settings in pairs(servers) do
    lsp[server].setup { on_attach = on_attach, settings = settings }
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = function(...)
    vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
        virtual_text = false
    })(...)
    vim.lsp.diagnostic.set_loclist({open_loclist = false})
end
EOF

hi LspDiagnosticsDefaultError ctermfg=1 ctermbg=10
hi LspDiagnosticsDefaultWarning ctermfg=3 ctermbg=10
hi LspDiagnosticsDefaultInformation ctermfg=4 ctermbg=10
