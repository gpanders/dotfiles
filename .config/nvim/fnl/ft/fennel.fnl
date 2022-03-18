(local efm "%C%[%^^]%#,%E%>Parse error in %f:%l,%E%>Compile error in %f:%l,%-Z%p^%.%#,%C%\\s%#%m,%-G* %.%#")
(local ns (nvim.create_namespace "ft/fennel"))

(local globals (icollect [name (pairs _G)] name))
(table.insert globals :vim)

(fn make-temp-file [text]
  (let [name (vim.fn.tempname)]
    (with-open [f (io.open name :w)]
      (f:write text))
    name))

(fn parse [output]
  (let [lines (vim.split output "\n")
        {: items} (vim.fn.getqflist {: efm : lines})]
    (each [_ v (ipairs items)]
      (set v.text (v.text:gsub "^\n" "")))
    (vim.diagnostic.fromqflist items)))

(fn compile-buffer [bufnr]
  (let [fennel (require :fennel)
        macro-path fennel.macro-path
        lines (nvim.buf.get_lines bufnr 0 -1 true)
        text (.. "(require-macros :macros)" (table.concat lines "\n"))
        temp (make-temp-file text)]
    (set fennel.macro-path (.. macro-path ";" (vim.fn.stdpath :config) "/fnl/?.fnl"))
    (let [(ok? output) (pcall fennel.compileString text {:allowedGlobals globals :filename temp})]
      (vim.loop.fs_unlink temp)
      (set fennel.macro-path macro-path)
      (values ok? output))))

(fn lint []
  (let [bufnr nvim.current.buf
        results (match (compile-buffer bufnr)
                    (false output) (parse output)
                    _ [])]
    (vim.diagnostic.set ns bufnr results)))

{: lint}
