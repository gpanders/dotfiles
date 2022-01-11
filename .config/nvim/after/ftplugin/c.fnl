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

(setlocal keywordprg ":vert Man")
(append! vim.b.undo_ftplugin " kp<")

(if (and (= 1 (vim.fn.executable "uncrustify")) (os.getenv :UNCRUSTIFY_CONFIG))
    (setlocal formatprg "uncrustify -l c 2>/dev/null")
    (= 1 (vim.fn.executable "clang-format"))
    (setlocal formatprg "clang-format -style=file -fallback-style=none"))

(when (not= vim.bo.formatprg "")
  (append! vim.b.undo_ftplugin " fp<"))

(autocmd ft/c :BufWritePost "<buffer>" (tags true))
(append! vim.b.undo_ftplugin "|au! ft/c")
