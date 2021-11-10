(setlocal commentstring "//%s")
(setlocal textwidth 100)
(append! vim.b.undo_ftplugin "|setl cms< tw<")

(when (= (vim.fn.executable :zig) 1)
  (setlocal formatprg "zig fmt --stdin")
  (append! vim.b.undo_ftplugin " fp<"))
