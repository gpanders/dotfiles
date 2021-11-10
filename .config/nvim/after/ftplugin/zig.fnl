(setlocal commentstring "//%s")
(append! vim.b.undo_ftplugin "|setl cms<")

(when (= (vim.fn.executable :zig) 1)
  (setlocal formatprg "zig fmt --stdin")
  (append! vim.b.undo_ftplugin " fp<"))
