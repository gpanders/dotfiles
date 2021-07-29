function! s:load()
    packadd nvim-lspconfig
    if !exists('g:lspconfig')
        return
    endif

lua <<
(function()
    local ns = vim.api.nvim_create_namespace("diagnostics")
    local clear = false
    function vim.lsp.diagnostic.show_virtual_text()
        if vim.fn.mode() ~= "n" then
            return
        end

        local items = vim.lsp.diagnostic.get_line_diagnostics()
        if clear then
            vim.api.nvim_buf_clear_namespace(0, ns, 0, -1)
            clear = false
        end

        if #items > 0 then
            local pos = vim.api.nvim_win_get_cursor(0)
            local line = pos[1] - 1
            local chunks = vim.lsp.diagnostic.get_virtual_text_chunks_for_line(0, line, items)
            vim.api.nvim_buf_set_extmark(0, ns, line, 0, {
                virt_text = chunks,
            })
            clear = true
        end
    end
end)()

local function on_attach(client, bufnr)
    vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

    local opts = { noremap = true, silent = true }

    vim.api.nvim_buf_set_keymap(bufnr, "n", "<C-]>", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<Cmd>lua vim.lsp.buf.references()<CR>", opts)
    vim.api.nvim_buf_set_keymap(bufnr, "n", "gR", "<Cmd>lua vim.lsp.buf.rename()<CR>", opts)

    vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
        vim.lsp.diagnostic.on_publish_diagnostics, {
            virtual_text = false,
            underline = false,
        }
    )

    vim.api.nvim_command(
        string.format(
            "autocmd CursorMoved,CursorHold <buffer=%d> lua vim.lsp.diagnostic.show_virtual_text()",
            bufnr
        )
    )

    vim.api.nvim_command("doautocmd User LspAttached")
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
    pylsp = {
        configurationSources = { "flake8" },
    },
}

for server_name, settings in pairs(servers) do
    local config = require("lspconfig")[server_name]
    config.setup({
        on_attach = on_attach,
        settings = { [server_name] = settings },
        flags = {
            debounce_text_changes = 150,
        },
    })

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
    autocmd User LspDiagnosticsChanged lua vim.lsp.diagnostic.set_loclist({ open_loclist = false })
augroup END
