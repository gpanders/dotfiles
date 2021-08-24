(local blacklist [:markdown :text :mail :gitsendemail])

(fn highlight-trailing-whitespace []
  (match [vim.o.readonly vim.o.modifiable vim.bo.filetype]
    (where [false true ft] (not= ft "")) (when (not (vim.tbl_contains blacklist ft))
                                           (exec "syn match TrailingWhitespace /\\\\\\@<!\\s\\+\\%#\\@<!$/ containedin=ALL"))
    _ (exec "syn clear TrailingWhitespace")))

(autocmd :highlight-trailing-whitespace :Syntax "*" [] (highlight-trailing-whitespace))
(autocmd :highlight-trailing-whitespace :OptionSet "readonly,modifiable" [] (highlight-trailing-whitespace))

(exec "hi link TrailingWhitespace Error")
