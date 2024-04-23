(set vim.bo.formatprg "zig fmt --stdin")

; Use 80 for textwidth to wrap comments, but set colorcolumn to 100 for code
(set vim.bo.textwidth 80)
(set vim.wo.colorcolumn "100")

(match vim.g.zig_tags_file
  tags (when (= 1 (vim.fn.filereadable tags))
         (set vim.bo.tags (.. vim.o.tags "," tags))))
