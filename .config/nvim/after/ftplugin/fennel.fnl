(setlocal commentstring ";%s")
(setlocal comments ":;")
(setlocal lisp)
(setlocal lispwords [:accumulate :collect :do :doto :each :fn :for :icollect :lambda :let :macro :macros :match :when :while :with-open])

; Custom macros for Neovim
(setlocal+= lispwords [:augroup :autocmd :with-module])

(append! vim.b.undo_ftplugin "|setl cms< com< lisp< lw<")

(when (> (vim.fn.executable "fnlfmt") 0)
  (setlocal formatprg "fnlfmt -")
  (append! vim.b.undo_ftplugin " fp<"))

(autocmd ft-fennel :BufWritePost "<buffer>"
  (local {: lint} (require "ft/fennel"))
  (lint)
  (autocmd ft-fennel :TextChanged "<buffer>" (lint)))

(append! vim.b.undo_ftplugin "|au! ft-fennel")
