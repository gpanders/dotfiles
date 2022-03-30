(vim.diagnostic.config {:virtual_text false
                        :underline true
                        :severity_sort true})

(augroup diagnostics#
  (autocmd :DiagnosticChanged "*"
    (let [bufnr (tonumber (vim.fn.expand "<abuf>"))]
      (when (nvim.buf.is_loaded bufnr)
        (let [diagnostics (vim.diagnostic.toqflist (vim.diagnostic.get bufnr))]
          (each [_ winid (ipairs (vim.fn.win_findbuf bufnr))]
            (let [action (match (vim.fn.getloclist winid {:context 1})
                           {:context {:diagnostics true}} "r"
                           _ " ")]
              (vim.fn.setloclist winid [] action {:items diagnostics
                                                  :title "Diagnostics"
                                                  :context {:diagnostics true}}))))))))
