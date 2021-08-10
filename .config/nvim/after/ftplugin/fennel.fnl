(setlocal commentstring ";%s")
(setlocal comments ":;")

(append! vim.b.undo_ftplugin "|setl cms< com<")

(when (> (vim.fn.executable "fnlfmt") 0)
  (setlocal formatprg "fnlfmt -")
  (append! vim.b.undo_ftplugin " fp<"))
