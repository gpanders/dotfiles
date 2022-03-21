(local lang-has-parser
       (let [lut {}]
         (fn [lang]
           (match (. lut lang)
             nil (let [has-parser (vim.treesitter.language.require_language lang nil true)]
                   (tset lut lang has-parser)
                   has-parser)
             v v))))

(fn root [bufnr]
  (let [bufnr (or bufnr nvim.current.buf)]
    (match (pcall vim.treesitter.get_parser bufnr)
      (true parser) (let [[tree] (parser:parse)]
                      (tree:root))
      _ nil)))

(fn named-children [node]
  (let [count (node:named_child_count)]
    (var i 1)
    (fn []
      (if (< count i)
          nil
          (let [child (node:named_child i)]
            (set i (+ i 1))
            child)))))

(fn contains-node? [node other]
  (let [(start-row start-col end-row end-col) (node:range)
        (other-start-row other-start-col other-end-row other-end-col) (other:range)]
    (if (< start-row other-start-row)
        (or (< other-end-row end-row)
            (and (= end-row other-end-row) (<= other-end-col end-col)))
        (and (= start-row other-start-row) (<= other-end-col end-col)))))

(fn find-node [start type]
  (var node nil)
  (each [child (named-children start) :until node]
    (if (= (child:type) type)
        (set node child)))
  (when (not node)
    (each [child (named-children start) :until node]
      (match (find-node child type)
        grandchild (set node grandchild))))
  node)

(fn node-at-cursor []
  (let [bufnr nvim.current.buf]
    (match (root bufnr)
      root-node (let [[lnum col] (nvim.win.get_cursor 0)
                      lnum (- lnum 1)]
                  (root-node:named_descendant_for_range lnum col lnum col)))))

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
            (end-row end-col) (cursor-node:end_)
            [context-id] (icollect [i v (ipairs query.captures)]
                           (if (= v :context) i))]
        (var done? false)
        (each [id node (query:iter_captures (root bufnr)) :until done?]
          (when (and (= id context-id) (contains-node? node cursor-node))
            (table.insert scopes node))
          (let [(start-row start-col) (node:start)]
            (set done? (or (< end-row start-row)
                           (and (= end-row start-row) (< end-col start-col))))))))
    scopes))

(fn context-text [bufnr node ?query]
  (let [query (or ?query (vim.treesitter.get_query (. vim.bo bufnr :filetype) :context))
        (start-row start-col) (node:start)
        (end-row end-col) (if (< 1 (length query.captures))
                              (let [[end-node] (icollect [id subnode (query:iter_captures node)]
                                                 (if (= (. query.captures id) :context.end) subnode))]
                                (end-node:end_))
                              (values (+ start-row 1) 0))]
    (-> (nvim.buf.get_text bufnr start-row 0 end-row end-col {})
        (table.concat " ")
        (string.gsub "%s+" " ")
        (->> (pick-values 1)))))

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
 : context-text
 : highlight-node
 : goto-node
 : root
 : commands
 : lang-has-parser}
