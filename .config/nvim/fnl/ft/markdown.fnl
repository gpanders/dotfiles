(fn publish-diagnostics [bufnr diagnostics]
  (let [params {:uri (vim.uri_from_bufnr bufnr) : diagnostics}
        method "textDocument/publishDiagnostics"
        {method handler} vim.lsp.handlers]
    (handler nil method params bufnr)))

(fn make-diagnostic [line text ?severity]
  (let [position {: line :character 0}]
    {:range {:start position :end position}
     :message text
     :severity (. vim.lsp.protocol.DiagnosticSeverity (or ?severity :Error))}))

(fn check-for-bad-links [bufnr]
  (let [lines (vim.api.nvim_buf_get_lines bufnr 0 -1 true)
        text (table.concat lines "\n")
        defined {}
        used {}]
    (each [i v (ipairs lines)]
      (each [link (v:gmatch "%[[^]]+%]%[([^]]+)%]")] ; [foo][bar]
        (tset used link i))
      (each [link (v:gmatch "%[([^]]+)%]%[%]")] ; [foo][]
        (tset used link i))
      (each [link (v:gmatch "%[([^]]+)%][^[(]")] ; [foo]
        (tset used link i))
      (match (v:match "^%s*%[([^]]+)%]: ")
        link (tset defined link i)))
    (local diagnostics [])
    (icollect [link line (pairs defined) :into diagnostics]
      (when (not (. used link))
        (make-diagnostic (- line 1) (.. "Unused link: " link) :Warning)))
    (icollect [link line (pairs used) :into diagnostics]
      (when (not (. defined link))
        (make-diagnostic (- line 1) (.. "Undefined link: " link) :Warning)))
    (publish-diagnostics bufnr diagnostics)))

(fn lint [bufnr]
  (check-for-bad-links bufnr))

{: lint}
