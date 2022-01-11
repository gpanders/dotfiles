(setlocal commentstring "#%s")
(append! vim.b.undo_ftplugin "|setl cms<")

(when (= 1 (vim.fn.executable :fish_indent))
  (setlocal formatprg :fish_indent)
  (append! vim.b.undo_ftplugin " fp<"))
