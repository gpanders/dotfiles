(autocmd lint# :BufWritePre "*" {:once true}
  #(let [lint (require :lint)
         linters (collect [k v (pairs {:sh ["shellcheck"]
                                       :python ["ruff"]})]
                   (values k (icollect [_ v (ipairs v)]
                               (if (= 1 (vim.fn.executable v)) v))))]
     (set lint.linters_by_ft linters)
     (autocmd lint# :BufWritePost "*" #(lint.try_lint))))
