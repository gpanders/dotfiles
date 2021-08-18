(local {: format-expr} (require "ft/csv"))

(global csvfex format-expr)

(setlocal formatexpr "v:lua.csvfex()")
(append! vim.b.undo_ftplugin "|setl fex<|lua csvfex = nil")
