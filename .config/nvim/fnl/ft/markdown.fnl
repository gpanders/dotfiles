(fn publish-diagnostics [bufnr diagnostics]
  (let [params {:uri (vim.uri_from_bufnr bufnr) : diagnostics}
        method "textDocument/publishDiagnostics"
        {method handler} vim.lsp.handlers]
    (handler nil method params bufnr)))

(fn make-diagnostic [pos text ?severity]
  (let [[lnum start-col end-col] pos
        line (- lnum 1)
        start {: line :character (- start-col 1)}
        end {: line :character (- end-col 1)}]
    {:range {: start : end}
     :message text
     :severity (. vim.lsp.protocol.DiagnosticSeverity (or ?severity :Error))}))

(fn check-for-bad-links [bufnr]
  (let [lines (vim.api.nvim_buf_get_lines bufnr 0 -1 true)
        text (table.concat lines "\n")
        defined {}
        used {}]
    (each [i v (ipairs lines)]
      (each [start link end (v:gmatch "%[[^]]+%]%[()([^]]+)()%]")] ; [foo][bar]
        (tinsert used link [i start end]))
      (each [start link end (v:gmatch "%[()([^]]+)()%]%[%]")] ; [foo][]
        (tinsert used link [i start end]))
      (each [start link end (v:gmatch "[^]]%[()([^]]+)()%][^[(]")] ; [foo]
        (tinsert used link [i start end]))
      (match (v:match "^%s*%[()([^]]+)()%]: ")
        (start link end) (tinsert defined link [i start end])))
    (local diagnostics [])
    (each [link positions (pairs defined)]
      (when (not (. used link))
        (each [_ pos (ipairs positions)]
          (table.insert diagnostics (make-diagnostic pos (.. "Unused link: " link) :Warning)))))
    (each [link positions (pairs used)]
      (when (not (. defined link))
        (each [_ pos (ipairs positions)]
          (table.insert diagnostics (make-diagnostic pos (.. "Undefined link: " link) :Warning)))))
    (publish-diagnostics bufnr diagnostics)))

(fn lint [bufnr]
  (check-for-bad-links bufnr))

{: lint}
