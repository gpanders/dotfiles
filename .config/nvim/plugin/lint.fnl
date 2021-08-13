(autocmd :lint :BufWritePre "*" :once
  (with-module [lint :lint]
    (set lint.linters_by_ft {
      :sh [ "shellcheck" ]
      :vim [ "vint" ]
      :lua [ "luacheck" ]
      :nix [ "nix" ]
      :python [ "flake8" ]
    })
    (autocmd :lint :BufWritePost "*" [] (lint.try_lint))))
