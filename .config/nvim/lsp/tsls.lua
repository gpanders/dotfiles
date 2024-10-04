return {
    filetype = "typescript",
    cmd = { "typescript-language-server", "--stdio" },
    root_dir = vim.fs.root(0, { "pnpm-lock.yaml", "tsconfig.json" }),
}
