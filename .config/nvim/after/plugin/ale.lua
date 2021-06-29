if not vim.g.loaded_ale then
    return
end

local ale_diagnostic_severity_map = {
    [vim.lsp.protocol.DiagnosticSeverity.Error] = 'E',
    [vim.lsp.protocol.DiagnosticSeverity.Warning] = 'W',
    [vim.lsp.protocol.DiagnosticSeverity.Information] = 'I',
    [vim.lsp.protocol.DiagnosticSeverity.Hint] = 'I',
}

vim.lsp.diagnostic.clear = function(bufnr, ...)
    vim.api.nvim_call_function('ale#other_source#ShowResults', { bufnr, 'nvim-lsp', {} })
end

vim.lsp.diagnostic.display = function(diagnostics, bufnr, ...)
    vim.lsp.diagnostic.clear(bufnr, ...)

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
        vim.api.nvim_call_function('ale#other_source#ShowResults', { bufnr, 'nvim-lsp', items })
    end
end

vim.g.ale_virtualtext_cursor = 1
vim.g.ale_floating_preview = 1

vim.api.nvim_set_keymap('n', 'gh', '<Cmd>ALEDetail<CR>', { noremap = true })
