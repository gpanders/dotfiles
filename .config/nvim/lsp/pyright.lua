return {
    filetype = "python",
    cmd = { "pyright-langserver", "--stdio" },
    name = "pyright",
    root = { "pyproject.toml", "setup.py", "setup.cfg", "requirements.txt", "Pipfile", "pyrightconfig.json" },
    settings = {
        python = {
            analysis = {
                diagnosticMode = "openFilesOnly",
                logLevel = "Warning",
            },
        },
    },
}
