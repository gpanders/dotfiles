function! s:load()
    packadd nvim-lspconfig
    if !exists('g:lspconfig')
        return
    endif

lua <<
local on_attach = function(client, bufnr)
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

    local opts = { noremap = true, silent = true }

    vim.api.nvim_buf_set_keymap(bufnr, "n", "<C-]>", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<Cmd>lua vim.lsp.buf.references()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gR", "<Cmd>lua vim.lsp.buf.rename()<CR>", opts)

    vim.cmd "doautocmd User LspAttached"
end

local servers = {
    rust_analyzer = {},
    clangd = {},
    gopls = {
        analyses = {
            unusedparams = true,
            unusedwrite = true,
            nilness = true,
        },
    },
    pyls = {
        configurationSources = { "flake8" },
    },
}

for server_name, settings in pairs(servers) do
    local config = require("lspconfig")[server_name]
    config.setup {
        on_attach = on_attach,
        settings = { server_name = settings },
        flags = {
            debounce_text_changes = 150,
        },
    }

    -- Start the server for the current buffer
    if vim.tbl_contains(config.filetypes, vim.bo.filetype) then
        config.autostart()
    end
end
.
endfunction

augroup load_lspconfig
    autocmd!
    autocmd FileType go,c,cpp,rust,python ++once call s:load()
augroup END
