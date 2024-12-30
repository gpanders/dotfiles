return {
    filetypes = { "go", "gomod", "gowork", "gosum" },
    cmd = { "gopls" },
    root_dir = function(cb)
        local root = vim.fs.root(0, { "go.mod" })
        if not root then
            return cb(root)
        end
        local workspace_root = vim.fs.root(root, { "go.work" })
        if workspace_root then
            return cb(workspace_root)
        end
        return cb(root)
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
