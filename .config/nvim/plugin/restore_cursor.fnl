(local ignored {:gitcommit true
                :gitrebase true
                :xxd true})

(autocmd restore-cursor# :BufRead "*"
  "Restore cursor position when a buffer is opened"
  (fn [{: buf}]
    (let [ft (. vim.bo buf :filetype)]
      (when (not (. ignored ft))
        (exec "silent! normal! g`\"")))))
