(set vim.bo.formatprg "rustfmt -q --emit=stdout")
(let [undo-ftplugin vim.b.undo_ftplugin]
  (set vim.b.undo_ftplugin (.. undo-ftplugin "|setl fp<")))
