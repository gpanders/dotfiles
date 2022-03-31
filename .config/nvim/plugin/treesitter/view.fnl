(local ns (nvim.create_namespace "treesitter/view"))
(local {: root : commands : node-at-cursor} (require :treesitter))

(local Tree {})

(fn Tree.new [self bufnr]
  (fn traverse [node depth tree]
    (each [child field (node:iter_children)]
      (let [id (child:id)
            type (child:type)
            (lnum col end-lnum end-col) (child:range)
            named (child:named)
            text (if named
                     (if field
                         (: "%s: (%s)" :format field type)
                         (: "(%s)" :format type))
                     (: "\"%s\"" :format (type:gsub "\n" "\\n")))]
        (table.insert tree {: id
                            : text
                            : named
                            : depth
                            : lnum
                            : col
                            :end_lnum end-lnum
                            :end_col end-col})
        (traverse child (+ depth 1) tree)))
    tree)
  (let [start-node (root bufnr)
        nodes (traverse start-node 0 [])
        named (icollect [_ v (ipairs nodes)]
                (if v.named v))
        t {: nodes : named :opts {:anon false}}]
    (setmetatable t self)
    (set self.__index self)
    t))

(fn Tree.draw [self bufnr]
  (tset vim.bo bufnr :modifiable true)
  (let [lines (icollect [_ item (self:iter)]
                (..
                  (: "  " :rep item.depth)
                  item.text
                  (if (= item.lnum item.end_lnum)
                      (: " [%d:%d-%d]" :format (+ item.lnum 1) (+ item.col 1) item.end_col)
                      (: " [%d:%d-%d:%d]" :format (+ item.lnum 1)
                                                  (+ item.col 1)
                                                  (+ item.end_lnum 1)
                                                  item.end_col))))]
    (nvim.buf.set_lines bufnr 0 -1 false lines))
  (tset vim.bo bufnr :modifiable false))

(fn Tree.get [self i]
  (let [t (if self.opts.anon self.nodes self.named)]
    (. t i)))

(fn Tree.toggle-anonymous-nodes [self]
  (set self.opts.anon (not self.opts.anon)))

(fn Tree.iter [self]
  (ipairs (if self.opts.anon self.nodes self.named)))

(fn commands.view []
  (let [buf nvim.current.buf
        win nvim.current.win]
    (var tree (Tree:new buf.id))
    (vim.cmd "topleft 60vnew")
    (let [w nvim.current.win
          b (Buffer.new (w:get_buf))]
      (tset vim.wo w.id :scrolloff 5)
      (tset vim.wo w.id :wrap false)
      (tset vim.bo b.id :buflisted false)
      (tset vim.bo b.id :buftype :nofile)
      (tset vim.bo b.id :bufhidden :wipe)
      (b:set_name (: "Syntax tree for %s" :format (vim.fn.fnamemodify (buf:get_name) ":.")))
      (tree:draw b.id)
      (vim.fn.matchadd :NonText "\\[[0-9:-]\\+\\]")
      (vim.fn.matchadd :String "\".*\"")
      (buf:clear_namespace ns 0 -1)
      (keymap :n "<CR>" (fn []
                            (let [[row] (w:get_cursor)
                                  {: lnum : col} (tree:get row)]
                              (set nvim.current.win win.id)
                              (win:set_cursor [(+ lnum 1) col]))) {:buffer b.id})
      (keymap :n "a" (fn []
                       (tree:toggle-anonymous-nodes)
                       (tree:draw b.id)) {:buffer b.id})
      (augroup treesitter#view
        (autocmd :CursorMoved {:buffer b.id}
          (buf:clear_namespace ns 0 -1)
          (let [[row] (w:get_cursor)
                {: lnum : col :end_lnum end-lnum :end_col end-col} (tree:get row)]
            (buf:set_extmark ns lnum col {:end_row end-lnum
                                          :end_col (math.max 0 end-col)
                                          :hl_group :Visual})))
        (autocmd :CursorMoved {:buffer buf.id}
          (if (not (b:is_loaded))
              true
              (let [cursor-node-id (: (node-at-cursor) :id)]
                (b:clear_namespace ns 0 -1)
                (var done? false)
                (each [i v (tree:iter) :until done?]
                  (when (= v.id cursor-node-id)
                    (set done? true)
                    (let [start (* 2 v.depth)
                          end (+ start (length v.text))]
                      (b:set_extmark ns (- i 1) start {:end_col end
                                                       :hl_group :Visual}))
                    (w:set_cursor [i 0]))))))
        (autocmd [:TextChanged :InsertLeave] {:buffer buf.id}
          (if (not (b:is_loaded))
              true
              (do
                (set tree (Tree:new buf.id))
                (tree:draw b.id))))
        (autocmd :BufLeave {:buffer b.id}
          (buf:clear_namespace ns 0 -1))
        (autocmd :BufHidden {:buffer buf.id :once true}
          (when (w:is_valid)
            (w:close true)))))))
