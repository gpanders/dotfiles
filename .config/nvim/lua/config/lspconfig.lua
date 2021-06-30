vim.cmd [[packadd nvim-lspconfig]]

if not vim.g.lspconfig then
    return
end

local on_attach = function(client, bufnr)
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    local opts = { noremap = true, silent = true }

    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-]>', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<Cmd>lua vim.lsp.buf.references()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gR', '<Cmd>lua vim.lsp.buf.rename()<CR>', opts)

    vim.cmd 'doautocmd User LspAttached'
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
    local config = require('lspconfig')[server]
    config.setup {
        on_attach = on_attach,
        settings = settings,
        flags = {
            debounce_text_changes = 150,
        },
    }

    -- Start the server for the current buffer
    if vim.tbl_contains(config.filetypes, vim.bo.filetype) then
        config.autostart()
    end
end
