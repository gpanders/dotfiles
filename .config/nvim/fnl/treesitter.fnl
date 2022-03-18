(local lang-has-parser
       (let [lut {}]
         (fn [lang]
           (match (. lut lang)
             nil (let [has-parser (vim.treesitter.language.require_language lang nil true)]
                   (tset lut lang has-parser)
                   has-parser)
             v v))))

(fn node-at-cursor []
  (let [bufnr nvim.current.buf]
    (match (pcall vim.treesitter.get_parser bufnr)
      (true parser) (let [[tree] (parser:parse)
                          root (tree:root)
                          [lnum col] (nvim.win.get_cursor 0)
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
      (let [cursor-node (node-at-cursor)
            [context-id] (icollect [i v (ipairs query.captures)]
                           (if (= v :context) i))]
        (var node (cursor-node:parent))
        (while node
          (var done? false)
          (each [id subnode (query:iter_captures node) :until done?]
            (when (and (= id context-id) (= (subnode:id) (node:id)))
              (set done? true)
              (table.insert scopes 1 node)))
          (set node (node:parent)))))
    scopes))

(fn highlight-node [bufnr ns node]
  (let [(start-row start-col end-row end-col) (node:range)]
    (nvim.buf.set_extmark bufnr ns start-row start-col {:end_row end-row
                                                        :end_col end-col
                                                        :hl_group :Visual})))

(fn goto-node [node end?]
  (let [(start-row start-col end-row end-col) (node:range)
        (row col) (if end?
                      (values end-row end-col)
                      (values start-row start-col))]
    (nvim.win.set_cursor 0 [(+ row 1) col])))

(local commands {})

(let [ns (nvim.create_namespace "")
      state {}]
  (fn commands.cursor []
    (let [bufnr nvim.current.buf
          highlight-cursor-node (fn []
                                  (match (node-at-cursor)
                                    node (do
                                           (nvim.buf.clear_namespace bufnr ns 0 -1)
                                           (highlight-node bufnr ns node)
                                           (nvim.buf.set_extmark bufnr ns (node:end_) 0 {:virt_text [[(: "(%s)" :format (node:type)) "Comment"]]
                                                                                         :hl_mode :combine}))))]
      (set state.cursor (not state.cursor))
      (if state.cursor
          (do
            (highlight-cursor-node)
            (augroup treesitter#cursor
              (autocmd! :CursorMoved "<buffer>" (highlight-cursor-node))))
          (do
            (commands.clear)
            (augroup! treesitter#cursor)))))

  (fn commands.context []
    (let [bufnr nvim.current.buf]
      (nvim.buf.clear_namespace bufnr ns 0 -1)
      (match (context bufnr)
        ([ctx] _) (let [clear #(do (commands.clear)
                                   (nvim.del.augroup_by_name "treesitter#context"))]
                    (highlight-node bufnr ns ctx)
                    (augroup treesitter#context
                      (autocmd! [:BufLeave :CursorMoved] "<buffer>")
                      (autocmd :BufLeave "<buffer>" {:once true} clear)
                      (autocmd :CursorMoved "<buffer>"
                        (match nvim.current.buf
                          bufnr (let [[lnum] (nvim.win.get_cursor 0)
                                      lnum (- lnum 1)]
                                  (when (or (< lnum (ctx:start)) (< (ctx:end_) lnum))
                                    (clear)))
                          _ (clear))))
                    (print (ctx:sexpr)))
        _ (echo "No context found"))))

  (fn commands.clear []
    (nvim.buf.clear_namespace 0 ns 0 -1)))

{: node-at-cursor
 : context
 : highlight-node
 : goto-node
 : commands
 : lang-has-parser}
