(local efm "%EParse error in %f:%l,%ECompile error in %f:%l,%C%*\\s%m,%-C%p^,%-G%.%#")

(fn publish-diagnostics [bufnr diagnostics]
  (let [params {:uri (vim.uri_from_bufnr bufnr) : diagnostics}
        method "textDocument/publishDiagnostics"]
    ((. vim.lsp.handlers method) nil method params bufnr)))

(fn parse [output]
  (let [lines (vim.split output "\n")
        qflist (vim.fn.getqflist {: efm : lines})]
    (icollect [_ item (pairs qflist.items)]
      (when (= item.valid 1)
        (let [col (if (> item.col 0) (- item.col 1) 0)
              position {:line (- item.lnum 1) :character col}]
          {:range {:start position :end position} :message item.text})))))

(fn compile-buffer [bufnr]
  (let [filename (vim.api.nvim_buf_get_name bufnr)
        lines (vim.api.nvim_buf_get_lines bufnr 0 -1 true)
        fennel (require :fennel)]
    (append! fennel.macro-path (.. ";" (vim.fn.stdpath :config) "/fnl/?.fnl"))
    (table.insert lines 1 "(require-macros :macros)")
    (fennel.compileString (table.concat lines "\n") {: filename})))

(fn lint [bufnr]
  (let [results (match (pcall compile-buffer bufnr)
                    (false output) (parse output)
                    _ [])]
    (publish-diagnostics bufnr results)))

{: lint}
