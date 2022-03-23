(local ns (nvim.create_namespace "treesitter/view"))
(local {: root : commands : node-at-cursor} (require :treesitter))

(local Tree {})

(fn Tree.new [self bufnr start-node]
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
  (let [t {:items (traverse start-node 0 [])
           :opts {:anon false}}]
    (setmetatable t self)
    (set self.__index self)
    t))

(fn Tree.draw [self bufnr]
  (tset vim.bo bufnr :modifiable true)
  (nvim.buf.set_lines bufnr 0 -1 false (icollect [_ item (ipairs self.items)]
                                         (when (or item.named self.opts.anon)
                                           (..
                                             (: "  " :rep item.depth)
                                             item.text
                                             (if (= item.lnum item.end_lnum)
                                                 (: " [%d:%d-%d]" :format (+ item.lnum 1) (+ item.col 1) item.end_col)
                                                 (: " [%d:%d-%d:%d]" :format (+ item.lnum 1)
                                                                             (+ item.col 1)
                                                                             (+ item.end_lnum 1)
                                                                             item.end_col))))))
  (tset vim.bo bufnr :modifiable false))

(fn Tree.get [self item]
  (when (not self.named)
    (set self.named (icollect [_ v (ipairs self.items)]
                      (if v.named v))))
  (let [items (if self.opts.anon
                  self.items
                  self.named)]
    (. items item)))

(fn Tree.toggle-anonymous-nodes [self]
  (set self.opts.anon (not self.opts.anon)))

(fn commands.view []
  (let [bufnr nvim.current.buf
        winid nvim.current.win
        root-node (root bufnr)
        tree (Tree:new bufnr root-node)]
    (exec "topleft 60vnew")
    (let [w nvim.current.win
          b (nvim.win.get_buf w)]
      (tset vim.bo b :buflisted false)
      (tset vim.bo b :buftype :nofile)
      (tset vim.bo b :bufhidden :wipe)
      (nvim.buf.set_name b (: "Syntax tree for %s" :format (vim.fn.fnamemodify (nvim.buf.get_name bufnr) ":.")))
      (tree:draw b)
      (vim.fn.matchadd :NonText "\\[[0-9:-]\\+\\]")
      (vim.fn.matchadd :String "\".*\"")
      (nvim.buf.clear_namespace bufnr ns 0 -1)
      (keymap :n "<CR>" (fn []
                            (let [[row] (nvim.win.get_cursor w)
                                  {: lnum : col} (tree:get row)]
                              (set nvim.current.win winid)
                              (nvim.win.set_cursor winid [(+ lnum 1) col]))) {:buffer b})
      (keymap :n "a" (fn []
                       (tree:toggle-anonymous-nodes)
                       (tree:draw b)) {:buffer b})
      (augroup treesitter#view
        (autocmd :CursorMoved {:buffer b}
          (nvim.buf.clear_namespace bufnr ns 0 -1)
          (let [[row] (nvim.win.get_cursor w)
                {: lnum : col :end_lnum end-lnum :end_col end-col} (tree:get row)]
            (nvim.buf.set_extmark bufnr ns lnum col {:end_row end-lnum
                                                       :end_col (math.max 0 end-col)
                                                       :hl_group :Visual})))
        (autocmd :CursorMoved {:buffer bufnr}
          (if (not (nvim.buf.is_loaded b))
              true
              (let [cursor-node-id (: (node-at-cursor) :id)]
                (nvim.buf.clear_namespace b ns 0 -1)
                (each [i v (ipairs tree)]
                  (when (= v.id cursor-node-id)
                    (let [start (* 2 v.depth)
                          end (+ start (length v.text))]
                      (nvim.buf.set_extmark b ns (- i 1) start {:end_col end
                                                                :hl_group :Visual})))))))
        (autocmd :BufLeave {:buffer b}
          (nvim.buf.clear_namespace bufnr ns 0 -1))))))
