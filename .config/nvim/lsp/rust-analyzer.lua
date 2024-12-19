return {
    filetype = "rust",
    cmd = { "rust-analyzer" },
    root_dir = function()
        local manifests = vim.fs.find("Cargo.toml", {
            limit = math.huge,
            path = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ':p:h'),
            upward = true,
        })
        for i=#manifests, 1, -1 do
            local f = io.open(manifests[i], "r")
            if f and string.find(f:read("*a"), "%[workspace%]") then
                return vim.fs.dirname(manifests[i])
            end
        end
        return vim.fs.dirname(manifests[1])
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
