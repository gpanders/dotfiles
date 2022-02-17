(fn node-at-cursor []
  (let [bufnr (vim.api.nvim_get_current_buf)]
    (match (pcall vim.treesitter.get_parser bufnr)
      (true parser) (let [[tree] (parser:parse)
                          root (tree:root)
                          [lnum col] (vim.api.nvim_win_get_cursor 0)
                          lnum (- lnum 1)]
                      (root:named_descendant_for_range lnum col lnum col))
      _ nil)))

(fn parents [node]
  (var cur node)
  (fn []
    (if cur
        (let [parent (cur:parent)]
          (set cur parent)
          parent))))

(fn containing-function-node [bufnr]
  (var function-node nil)
  (var text nil)
  (let [lang (. vim.bo bufnr :filetype)]
    (match (vim.treesitter.query.get_query lang :function)
      query
      (do
        (var child (node-at-cursor))
        (each [parent (parents child) :until function-node]
          (each [id node (query:iter_captures parent)]
            (when (and (= (node:id) (child:id)) (= (. query.captures id) :function))
              (set function-node node)))
          (set child parent))

        (when function-node
          (var start nil)
          (var end nil)
          (each [id node (query:iter_captures function-node)]
            (match (. query.captures id)
              :function.start (set start (node:start))
              :function.end (set end (node:end_))))
          (let [start-row (or start (function-node:start))
                end-row (or end start-row)]
            (set text (table.concat (vim.api.nvim_buf_get_lines bufnr start-row (+ end-row 1) true) " ")))))))

  (values function-node text))

(fn highlight-node [bufnr ns node]
  (let [(start-row start-col end-row end-col) (node:range)]
    (vim.api.nvim_buf_set_extmark bufnr ns start-row start-col {:end_row end-row
                                                                :end_col end-col
                                                                :hl_group :Visual})))

{: node-at-cursor
 : containing-function-node
 : highlight-node}
