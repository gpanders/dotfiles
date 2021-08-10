(local {: set-path : tags} (require "ft/c"))

(setlocal commentstring "//%s")
(setlocal define&)
(setlocal include "^\\s*#\\s*include\\s*[\"<]\\@=")
(setlocal includeexpr&)
(setlocal textwidth 80)

; Support /// as a comment leader, used for writing Doxygen comments
(setlocal-= comments "://")
(setlocal+= comments [":///" "://"])

(set-path)
(tags)

(append! vim.b.undo_ftplugin "|setlocal cms< def< inc< inex< path< tw< com<")

(when (. (vim.api.nvim_get_commands {:builtin false}) :Man)
  (setlocal keywordprg ":Man")
  (append! vim.b.undo_ftplugin "|setlocal kp<"))

(when (> (vim.fn.executable "clang-format") 0)
  (setlocal formatprg "clang-format -style=file -fallback-style=none")
  (append! vim.b.undo_ftplugin "|setlocal fp<"))

(autocmd :ftplugin_c :BufWritePost "<buffer>" [] (tags true))

(append! vim.b.undo_ftplugin "|au! ftplugin_c")
