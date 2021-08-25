(autocmd :lint :BufWritePre "*" :once
  (with-module [lint :lint]
    (with-module [fennel "lint.linters.fennel"]
      (let [tmp (vim.fn.tempname)]
        (fn mktemp []
          (let [lines (vim.api.nvim_buf_get_lines 0 0 -1 true)]
            (table.insert lines 1 "(require-macros :macros)")
            (with-open [f (io.open tmp :w)]
              (f:write (table.concat lines "\n")))
            tmp))
        (table.insert fennel.args mktemp)
        (set fennel.append_fname false)
        (set fennel.env {:FENNEL_MACRO_PATH (.. (vim.fn.stdpath :config) "/fnl/?.fnl")})
        (set fennel.globals [:vim])
        (autocmd :lint :VimLeave "*"
          (vim.loop.fs_unlink tmp))))

    (set lint.linters_by_ft {:sh ["shellcheck"]
                             :vim ["vint"]
                             :lua ["luacheck"]
                             :nix ["nix"]
                             :python ["flake8"]
                             :fennel ["fennel"]})
    (autocmd :lint :BufWritePost "*" (lint.try_lint))))
