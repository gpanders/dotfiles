(local blacklist [:markdown :text :mail :gitsendemail])

(autocmd :highlight-trailing-whitespace :Syntax "*" []
         (match [vim.o.readonly vim.o.modifiable vim.bo.filetype]
           (where [false true ft] (not= ft ""))
             (when (not (vim.tbl_contains blacklist ft))
               (exec "syn match TrailingWhitespace /\\\\\\@<!\\s\\+\\%#\\@<!$/ containedin=ALL"))))

(exec "hi link TrailingWhitespace Error")
