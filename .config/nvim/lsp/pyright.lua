return {
    filetype = "python",
    cmd = { "pyright-langserver", "--stdio" },
    name = "pyright",
    root_dir = vim.fs.root(0, { "pyproject.toml", "setup.py", "setup.cfg", "requirements.txt", "Pipfile", "pyrightconfig.json" }),
    settings = {
        python = {
            analysis = {
                diagnosticMode = "openFilesOnly",
                logLevel = "Warning",
            },
        },
    },
}
