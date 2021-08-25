(setlocal commentstring ";%s")
(setlocal comments ":;")
(setlocal lisp)
(setlocal lispwords [:accumulate :collect :do :doto :each :fn :for :icollect :lambda :let :macro :macros :match :when :while :with-open])

; Custom macros for Neovim
(setlocal+= lispwords [:augroup :with-module])

(append! vim.b.undo_ftplugin "|setl cms< com< lisp< lw<")

(when (> (vim.fn.executable "fnlfmt") 0)
  (setlocal formatprg "fnlfmt -")
  (append! vim.b.undo_ftplugin " fp<"))
