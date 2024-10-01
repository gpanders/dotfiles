return {
    filetype = "typescript",
    cmd = { "typescript-language-server", "--stdio" },
    root = { "pnpm-lock.yaml", "tsconfig.json" },
}
