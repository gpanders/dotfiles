(autocmd lint# :BufWritePre "*" {:once true}
  #(with-module [lint :lint]
    (let [linters (collect [k v (pairs {:sh ["shellcheck"]
                                        :python ["ruff"]
                                        :zig ["zig"]})]
                    (values k (icollect [_ v (ipairs v)]
                                (if (= 1 (vim.fn.executable v)) v))))]
      (set lint.linters_by_ft linters)
      (autocmd lint# :BufWritePost "*" #(lint.try_lint)))))
