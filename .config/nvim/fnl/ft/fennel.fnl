(local efm "%C%[%^^]%#,%E%>Parse error in %f:%l,%E%>Compile error in %f:%l,%-Z%p^%.%#,%C%m,%-G* %.%#")
(local ns (vim.api.nvim_create_namespace "ft/fennel"))

(local globals (icollect [name (pairs _G)] name))
(table.insert globals :vim)

(fn make-temp-file [text]
  (let [name (vim.fn.tempname)]
    (with-open [f (io.open name :w)]
      (f:write text))
    name))

(fn set-diagnostics [bufnr diagnostics]
  (each [_ d (pairs diagnostics)]
    (set d.lnum (- d.lnum 1)))
  (vim.diagnostic.set ns bufnr diagnostics))

(fn parse [output]
  (let [lines (vim.split output "\n")
        qflist (vim.fn.getqflist {: efm : lines})]
    (vim.diagnostic.fromqflist qflist.items)))

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

(fn lint []
  (let [bufnr (vim.api.nvim_get_current_buf)
        results (match (pcall compile-buffer bufnr)
                    (false output) (parse output)
                    _ [])]
    (set-diagnostics bufnr results)))

{: lint}
