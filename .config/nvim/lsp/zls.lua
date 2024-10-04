return {
    filetype = "zig",
    cmd = { "zls" },
    root_dir = vim.fs.root(0, { "build.zig" }),
}
