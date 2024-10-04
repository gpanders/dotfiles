return {
    filetype = "ocaml",
    cmd = { "ocamllsp" },
    root_dir = vim.fs.root(0, { "*.opam", "dune-project", "dune-workspace" }),
}
