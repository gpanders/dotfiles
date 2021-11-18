(setlocal commentstring "#%s")
(append! vim.b.undo_ftplugin "|setl cms<")

(when (= (vim.fn.executable :fish_indent) 1)
  (setlocal formatprg :fish_indent)
  (append! vim.b.undo_ftplugin " fp<"))
