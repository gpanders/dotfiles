(setlocal commentstring "//%s")
(setlocal textwidth 100)
(setlocal-= formatoptions :t)
(append! vim.b.undo_ftplugin "|setl cms< tw< fo<")

(when (= (vim.fn.executable :zig) 1)
  (setlocal formatprg "zig fmt --stdin")
  (append! vim.b.undo_ftplugin " fp<"))
