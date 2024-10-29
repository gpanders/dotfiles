return {
    filetype = { "c", "cpp" },
    cmd = { "clangd", "--background-index" },
    root_dir = vim.fs.root(0, { ".clangd", ".clang-format", "compile_commands.json", "compile_flags.txt" }),
    flags = {
        debounce_text_changes = 20,
    },
    capabilities = {
        textDocument = {
            completion = {
                editsNearCursor = true,
            },
        },
        offsetEncoding = { "utf-8", "utf-16" },
    },
}
