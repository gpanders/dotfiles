(local ns (nvim.create_namespace "treesitter/view"))
(local {: root : commands : node-at-cursor} (require :treesitter))

(fn traverse-tree [bufnr node depth tree]
  (each [child field (node:iter_children)]
    (when (child:named)
      (let [type (child:type)
            (lnum col end-lnum end-col) (child:range)]
        (table.insert tree {:id (child:id)
                            :text (if field
                                      (: "%s: (%s)" :format field type)
                                      (: "(%s)" :format type))
                            : depth
                            : lnum
                            : col
                            :end_lnum end-lnum
                            :end_col end-col})
        (traverse-tree bufnr child (+ depth 1) tree)))))

(fn commands.view []
  (let [bufnr nvim.current.buf
        winid nvim.current.win
        items []
        root-node (root bufnr)]
    (traverse-tree bufnr root-node 0 items)
    (exec "topleft 60vnew")
    (let [w nvim.current.win
          b (nvim.win.get_buf w)]
      (tset vim.bo b :buflisted false)
      (tset vim.bo b :buftype :nofile)
      (tset vim.bo b :bufhidden :wipe)
      (nvim.buf.set_name b (: "Syntax tree for %s" :format (vim.fn.fnamemodify (nvim.buf.get_name bufnr) ":.")))
      (nvim.buf.set_lines b 0 -1 false (icollect [_ item (ipairs items)]
                                         (..
                                           (: "  " :rep item.depth)
                                           item.text
                                           (if (= item.lnum item.end_lnum)
                                               (: " [%d:%d-%d]" :format (+ item.lnum 1) (+ item.col 1) item.end_col)
                                               (: " [%d:%d-%d:%d]" :format (+ item.lnum 1)
                                                                           (+ item.col 1)
                                                                           (+ item.end_lnum 1)
                                                                           item.end_col)))))
      (tset vim.bo b :modifiable false)
      (vim.fn.matchadd :NonText "\\[[0-9:-]\\+\\]")
      (nvim.buf.clear_namespace bufnr ns 0 -1)
      (keymap :n "<CR>" (fn []
                            (let [[row] (nvim.win.get_cursor w)
                                  {: lnum : col} (. items row)]
                              (set nvim.current.win winid)
                              (nvim.win.set_cursor winid [(+ lnum 1) col]))) {:buffer b})
      (autocmd :CursorMoved {:buffer b}
        (nvim.buf.clear_namespace bufnr ns 0 -1)
        (let [[row] (nvim.win.get_cursor w)
              {: lnum : col :end_lnum end-lnum :end_col end-col} (. items row)]
          (nvim.buf.set_extmark bufnr ns lnum col {:end_row end-lnum
                                                     :end_col (math.max 0 end-col)
                                                     :hl_group :Visual})))
      (autocmd :CursorMoved {:buffer bufnr}
        (if (not (nvim.buf.is_loaded b))
            true
            (let [cursor-node-id (: (node-at-cursor) :id)]
              (nvim.buf.clear_namespace b ns 0 -1)
              (each [i v (ipairs items)]
                (when (= v.id cursor-node-id)
                  (let [start (* 2 v.depth)
                        end (+ start (length v.text))]
                    (nvim.buf.set_extmark b ns (- i 1) start {:end_col end
                                                              :hl_group :Visual})))))))
      (autocmd :BufLeave {:buffer b}
        (nvim.buf.clear_namespace bufnr ns 0 -1)))))
