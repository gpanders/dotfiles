(setlocal textwidth 79)
; https://github.com/tpope/vim-markdown/issues/134
(setlocal comments "n:>")
(setlocal formatlistpat "^\\s*\\d\\+\\.\\s\\+\\|^\\s*[-*+]\\s\\+\\|^\\[^\\ze[^\\]]\\+\\:")
(setlocal conceallevel 2)
(append! vim.b.undo_ftplugin "|setl tw< com< flp< cole<")

(when (= 1 (vim.fn.executable :pandoc))
  (exec "compiler pandoc"))

(when (= 2 (vim.fn.exists ":EvalBlock"))
  (keymap :n "Z!" "<Cmd>EvalBlock<CR>" {:buffer true})
  (append! vim.b.undo_ftplugin "|nun <buffer> Z!"))

(keymap :n "<CR>" "<Cmd>call ft#markdown#open('edit')<CR>" {:buffer true})
(keymap :n "<C-W><CR>" "<Cmd>call ft#markdown#open('split')<CR>" {:buffer true})
(append! vim.b.undo_ftplugin "|nun <buffer> <CR>|nun <buffer> <C-W><CR>")

(autocmd ft/markdown :BufWritePost "<buffer>"
  (local {: lint} (require "ft/markdown"))
  (lint nvim.current.buf))
(append! vim.b.undo_ftplugin "|au! ft/markdown")
