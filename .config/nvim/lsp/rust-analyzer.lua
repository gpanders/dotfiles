return {
    filetypes = { "rust" },
    cmd = { "rust-analyzer" },
    root_dir = function(cb)
        local root = vim.fs.root(0, { "Cargo.toml" })
        if not root then
            return cb(root)
        end

        local out = vim.system({"cargo", "metadata", "--no-deps", "--format-version", "1"}, { cwd = root }):wait()
        if out.code ~= 0 then
            return cb(root)
        end

        local ok, result = pcall(vim.json.decode, out.stdout)
        if ok and result.workspace_root then
            return cb(result.workspace_root)
        end

        return cb(root)
    end,
    settings = {
        autoformat = true,
        ["rust-analyzer"] = {
            check = {
                command = "clippy",
            },
        },
    },
}
