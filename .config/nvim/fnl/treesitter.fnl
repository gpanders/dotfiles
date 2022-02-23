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

(fn context [bufnr]
  (let [scopes []
        lines []
        lang (. vim.bo bufnr :filetype)]
    (match (vim.treesitter.query.get_query lang :context)
      query
      (do
        (var node (: (node-at-cursor) :parent))
        (while node
          (var done? false)
          (each [id subnode (query:iter_captures node) :until done?]
            (when (= (subnode:id) (node:id))
              (set done? true)
              (table.insert scopes node)
              (let [start-row (node:start)
                    [text] (vim.api.nvim_buf_get_lines bufnr start-row (+ start-row 1) true)]
                (table.insert lines 1 text))))
          (set node (node:parent)))))
    (let [lines (if (< 1 (length lines))
                    ; When there is more than one level of context, trim
                    ; whitespace and join them with a separator
                    (icollect [_ line (ipairs lines)]
                      (-> line
                          (vim.trim)
                          (string.gsub "%s*[%[%(%{]*%s*$" "")
                          (->> (pick-values 1))))
                    lines)]
      (values scopes (table.concat lines " -> ")))))

(fn highlight-node [bufnr ns node]
  (let [(start-row start-col end-row end-col) (node:range)]
    (vim.api.nvim_buf_set_extmark bufnr ns start-row start-col {:end_row end-row
                                                                :end_col end-col
                                                                :hl_group :Visual})))

(fn goto-node [node end?]
  (let [(start-row start-col end-row end-col) (node:range)
        (row col) (if end?
                      (values end-row end-col)
                      (values start-row start-col))]
    (vim.api.nvim_win_set_cursor 0 [(+ row 1) col])))

{: node-at-cursor
 : context
 : highlight-node
 : goto-node}
