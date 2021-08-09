(when (and (= vim.bo.filetype :html) (> (vim.fn.executable :tidy) 0))
  (set vim.bo.formatprg "tidy --quiet yes --show-body-only auto --show-info no --show-warnings no --indent auto --tidy-mark no --wrap 0")
  (set vim.b.undo_ftplugin (.. (or vim.b.undo_ftplugin "") "|setl fp<")))
