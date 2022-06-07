(autocmd zig# :LspAttach "<buffer>"
  #(match vim.g.zig_std_dir
     d (vim.lsp.buf.add_workspace_folder d)))

(set vim.bo.formatprg "zig fmt --stdin")
