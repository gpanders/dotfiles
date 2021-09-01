(local efm "%C%[%^^]%#,%E%>Parse error in %f:%l,%E%>Compile error in %f:%l,%-Z%p^%.%#,%C%m,%-G* %.%#")

(local globals (icollect [name (pairs _G)] name))
(table.insert globals :vim)

(fn make-temp-file [text]
  (let [name (vim.fn.tempname)]
    (with-open [f (io.open name :w)]
      (f:write text))
    name))

(fn publish-diagnostics [bufnr diagnostics]
  (let [params {:uri (vim.uri_from_bufnr bufnr) : diagnostics}
        method "textDocument/publishDiagnostics"
        {method handler} vim.lsp.handlers]
    (handler nil method params bufnr)))

(fn parse [output]
  (let [lines (vim.split output "\n")
        qflist (vim.fn.getqflist {: efm : lines})]
    (icollect [_ item (pairs qflist.items)]
      (when (= item.valid 1)
        (let [col (if (> item.col 0) (- item.col 1) 0)
              position {:line (- item.lnum 2) :character col}]
          {:range {:start position :end position} :message item.text})))))

(fn compile-buffer [bufnr]
  (let [fennel (require :fennel)
        macro-path fennel.macro-path
        lines (vim.api.nvim_buf_get_lines bufnr 0 -1 true)
        _ (table.insert lines 1 "(require-macros :macros)")
        text (table.concat lines "\n")
        temp (make-temp-file text)]
    (set fennel.macro-path (.. macro-path ";" (vim.fn.stdpath :config) "/fnl/?.fnl"))
    (let [output (fennel.compileString text {:allowedGlobals globals :filename temp})]
      (vim.loop.fs_unlink temp)
      (set fennel.macro-path macro-path)
      output)))

(fn lint [bufnr]
  (let [results (match (pcall compile-buffer bufnr)
                    (false output) (parse output)
                    _ [])]
    (publish-diagnostics bufnr results)))

{: lint}
