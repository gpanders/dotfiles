(local {: format-expr : max-column-width} (require "ft/csv"))

(global csvfex format-expr)

(setlocal formatexpr "v:lua.csvfex()")
(append! vim.b.undo_ftplugin "|setl fex<|call luaeval('csvfex = nil')")
