return {
    filetype = { "go", "gomod", "gowork", "gosum" },
    cmd = { "gopls" },
    root_dir = function()
        local root = vim.fs.root(0, { "go.mod" })
        local workspace_root = vim.fs.root(root, { "go.work" })
        if workspace_root then
            return workspace_root
        end
        return root
    end,
    settings = {
        autoformat = true,
        gopls = {
            analyses = {
                unusedparams = true,
                unusedwrite = true,
                nilness = true,
            },
        },
    },
}
