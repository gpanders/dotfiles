(when (and (= vim.bo.filetype :html) (> (vim.fn.executable :tidy) 0))
  (setlocal formatprg "tidy --quiet yes --show-body-only auto --show-info no --show-warnings no --indent auto --tidy-mark no --wrap 0")
  (append! vim.b.undo_ftplugin "|setlocal fp<"))
