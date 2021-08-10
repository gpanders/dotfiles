(setlocal commentstring ";%s")
(setlocal comments ":;")

(set vim.b.undo_ftplugin
  (.. (or vim.b.undo_ftplugin "") "|setl cms< com<"))

(when (> (vim.fn.executable "fnlfmt") 0)
  (setlocal formatprg "fnlfmt -")
  (set vim.b.undo_ftplugin
    (.. vim.b.undo_ftplugin " fp<")))
