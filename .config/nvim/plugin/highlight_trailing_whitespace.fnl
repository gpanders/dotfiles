(local blacklist [:markdown :text :mail :gitsendemail :man])

(fn highlight-trailing-whitespace []
  (match [vim.o.readonly vim.o.modifiable vim.bo.filetype vim.bo.buftype]
    (where [false true ft ""] (not= ft "")) (when (not (vim.tbl_contains blacklist ft))
                                              (exec "syn match TrailingWhitespace /\\\\\\@<!\\s\\+\\%#\\@<!$/ containedin=ALL"))
    _ (exec "silent! syn clear TrailingWhitespace")))

(augroup highlight-trailing-whitespace
  (autocmd :Syntax "*" (highlight-trailing-whitespace))
  (autocmd :OptionSet "readonly,modifiable" (highlight-trailing-whitespace)))
