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
              (table.insert scopes 1 node)))
          (set node (node:parent)))))
    scopes))

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

(local commands {})

(let [ns (vim.api.nvim_create_namespace "")
      state {}]
  (fn commands.cursor []
    (let [bufnr (vim.api.nvim_get_current_buf)
          highlight-cursor-node (fn []
                                  (match (node-at-cursor)
                                    node (do
                                           (vim.api.nvim_buf_clear_namespace bufnr ns 0 -1)
                                           (highlight-node bufnr ns node)
                                           (vim.api.nvim_buf_set_extmark bufnr ns (node:end_) 0 {:virt_text [[(: "(%s)" :format (node:type)) "Comment"]]
                                                                                                 :hl_mode :combine}))))]
      (set state.cursor (not state.cursor))
      (if state.cursor
          (do
            (highlight-cursor-node)
            (autocmd treesitter#cursor :CursorMoved "<buffer>" (highlight-cursor-node)))
          (do
            (commands.clear)
            (vim.api.nvim_del_augroup_by_name "treesitter#cursor")))))

  (fn commands.context []
    (let [bufnr (vim.api.nvim_get_current_buf)]
      (vim.api.nvim_buf_clear_namespace bufnr ns 0 -1)
      (match (context bufnr)
        ([ctx] _) (let [clear #(do (commands.clear)
                                   (vim.api.nvim_del_augroup_by_name "treesitter#context"))]
                    (highlight-node bufnr ns ctx)
                    (augroup treesitter#context
                      (autocmd :BufLeave "<buffer>" {:once true} clear)
                      (autocmd :CursorMoved "<buffer>"
                        (match (vim.api.nvim_get_current_buf)
                          bufnr (let [[lnum] (vim.api.nvim_win_get_cursor 0)
                                      lnum (- lnum 1)]
                                  (when (or (< lnum (ctx:start)) (< (ctx:end_) lnum))
                                    (clear)))
                          _ (clear))))
                    (print (ctx:sexpr)))
        _ (echo "No context found"))))

  (fn commands.clear []
    (vim.api.nvim_buf_clear_namespace 0 ns 0 -1)))

{: node-at-cursor
 : context
 : highlight-node
 : goto-node
 : commands}
