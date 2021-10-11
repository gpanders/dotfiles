(local ns (vim.api.nvim_create_namespace "ft/markdown"))

(fn tinsert [t key val]
  (do
    (when (not (. t key))
      (tset t key []))
    (table.insert (. t key) val)))

(fn set-diagnostics [bufnr diagnostics]
  (vim.diagnostic.set ns bufnr diagnostics))

(fn make-diagnostic [pos message ?severity]
  (let [[lnum col end-col] pos
        lnum (- lnum 1)
        col (- col 1)
        end-col (- end-col 1)]
    {: lnum : col :end_col end-col :severity (or ?severity :ERROR) : message}))

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
          (table.insert diagnostics (make-diagnostic pos (.. "Unused link: " link) :WARN)))))
    (each [link positions (pairs used)]
      (when (not (. defined link))
        (each [_ pos (ipairs positions)]
          (table.insert diagnostics (make-diagnostic pos (.. "Undefined link: " link) :WARN)))))
    (set-diagnostics bufnr diagnostics)))

(fn lint [bufnr]
  (check-for-bad-links bufnr))

{: lint}
