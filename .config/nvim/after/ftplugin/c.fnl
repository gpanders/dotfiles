(local {: set-path : tags} (require "ft/c"))

(setlocal commentstring "//%s")
(setlocal define "^\\s*#\\s*define")
(setlocal include "^\\s*#\\s*include\\s*[\"<]\\@=")
(setlocal textwidth 80)

; Support /// as a comment leader, used for writing Doxygen comments
(setlocal-= comments "://")
(setlocal+= comments [":///" "://"])

(set-path)
(tags)

(append! vim.b.undo_ftplugin "|setl cms< def< inc< path< tw< com<")

(when (. (vim.api.nvim_get_commands {:builtin false}) :Man)
  (setlocal keywordprg ":vert Man")
  (append! vim.b.undo_ftplugin " kp<"))

(if (and (> (vim.fn.executable "uncrustify") 0) (os.getenv :UNCRUSTIFY_CONFIG))
    (setlocal formatprg "uncrustify -l c 2>/dev/null")
    (> (vim.fn.executable "clang-format") 0)
    (setlocal formatprg "clang-format -style=file -fallback-style=none"))

(match (pcall #vim.bo.formatprg)
  true (append! vim.b.undo_ftplugin " fp<"))

(autocmd ft-c :BufWritePost "<buffer>" (tags true))

(append! vim.b.undo_ftplugin "|au! ft-c")
