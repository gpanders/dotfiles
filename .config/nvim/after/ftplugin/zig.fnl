(autocmd ft/zig :LspAttach "<buffer>"
  #(match vim.g.zig_std_dir
     d (silent (vim.lsp.buf.add_workspace_folder d))))

(set vim.bo.formatprg "zig fmt --stdin")

; Use 80 for textwidth to wrap comments, but set colorcolumn to 100 for code
(set vim.bo.textwidth 80)
(set vim.wo.colorcolumn "100")

(match vim.g.zig_tags_file
  tags (when (= 1 (vim.fn.filereadable tags))
         (set vim.bo.tags (.. vim.o.tags "," tags))))

(let [lsp (require :lsp)]
  (lsp.start {:cmd ["zls"]
              :root ["build.zig"]}))
