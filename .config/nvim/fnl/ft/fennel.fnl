(local globals [:vim])
(each [name (pairs _G)]
  (table.insert globals name))

(fn make-temp-file [text]
  (let [name (vim.fn.tempname)]
    (with-open [f (io.open name :w)]
      (f:write text))
    name))

(fn publish-diagnostics [bufnr diagnostics]
  (let [client-id 1441
        params {:uri (vim.uri_from_bufnr bufnr) : diagnostics}
        method "textDocument/publishDiagnostics"
        {method handler} vim.lsp.handlers]
    (handler nil params {: method :client_id client-id})))

(fn parse [output]
  (match (output:match "^%w+ error in .+:(%d+)[\n\r]%s+([^\n\r]+)\n.*\n(%s+)^+[\n\r]")
    (lnum message s) (let [lnum (- lnum 2)
                           col (length s)
                           position {:line lnum :character col}]
                       [{:range {:start position :end position} : message}])))

(fn compile-buffer [bufnr]
  (let [fennel (require :fennel)
        macro-path fennel.macro-path
        lines (vim.api.nvim_buf_get_lines bufnr 0 -1 false)
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
