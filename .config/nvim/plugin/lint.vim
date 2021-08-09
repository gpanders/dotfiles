function! s:load()
lua <<
local ok, lint = pcall(require, "lint")
if not ok then
    return
end

lint.linters_by_ft = {
    ["sh"] = { "shellcheck" },
    ["vim"] = { "vint" },
    ["lua"] = { "luacheck" },
    ["nix"] = { "nix" },
    ["python"] = { "flake8" },
}

vim.api.nvim_command("autocmd! lint BufWritePost * lua require('lint').try_lint()")
.
endfunction

augroup lint
    autocmd BufWritePre * ++once call s:load()
augroup END
