if !get(g:, 'loaded_ale')
    finish
endif

" Prevent LSP diagnostics from displaying when LSP is attached
function! s:on_lsp_attach()
    lua vim.lsp.diagnostic.display = function() end
    autocmd BufWrite <buffer> ++once call s:on_buf_write()
endfunction

" Display diagnostics after writing the buffer
function! s:on_buf_write()
lua <<
local ale_diagnostic_severity_map = {
    [vim.lsp.protocol.DiagnosticSeverity.Error] = 'E',
    [vim.lsp.protocol.DiagnosticSeverity.Warning] = 'W',
    [vim.lsp.protocol.DiagnosticSeverity.Information] = 'I',
    [vim.lsp.protocol.DiagnosticSeverity.Hint] = 'I',
}

vim.lsp.diagnostic.clear = function(bufnr, client_id, ...)
    local client = vim.lsp.get_client_by_id(client_id)
    vim.api.nvim_call_function('ale#other_source#ShowResults', { bufnr, client.name, {} })
end

vim.lsp.diagnostic.display = function(diagnostics, bufnr, client_id, ...)
    vim.lsp.diagnostic.clear(bufnr, client_id, ...)

    local client = vim.lsp.get_client_by_id(client_id)
    local items = {}
    for _, item in ipairs(diagnostics) do
        table.insert(items, {
            text = item.message,
            filename = vim.api.nvim_buf_get_name(bufnr),
            lnum = item.range.start.line + 1,
            end_lnum = item.range['end'].line,
            col = item.range.start.character + 1,
            end_col = item.range['end'].character,
            type = ale_diagnostic_severity_map[item.severity],
        })
    end

    if not vim.tbl_isempty(items) then
        vim.api.nvim_call_function('ale#other_source#ShowResults', {
            bufnr,
            client.name,
            items
        })
    end
end

local bufnr = vim.api.nvim_get_current_buf()
vim.lsp.for_each_buffer_client(bufnr, function(client)
    local diagnostics = vim.lsp.diagnostic.get(bufnr, client.id)
    vim.lsp.diagnostic.display(diagnostics, bufnr, client.id)
end)
.
endfunction

autocmd User LspAttached ++once call s:on_lsp_attach()

let g:ale_virtualtext_cursor = 1
let g:ale_floating_preview = 1

nnoremap gh <Cmd>ALEDetail<CR>
