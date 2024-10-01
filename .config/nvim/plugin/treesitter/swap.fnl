(fn swap-nodes [buf forward left right]
  "Swap the text of two nodes.
  The 'left' node should precede the 'right' node in the buffer (i.e. in a previous line or column)"
  (let [(left-start-row left-start-col left-end-row left-end-col) (left:range)
        (right-start-row right-start-col right-end-row right-end-col) (right:range)
        left-text (nvim.buf_get_text buf left-start-row left-start-col left-end-row left-end-col {})
        right-text (nvim.buf_get_text buf right-start-row right-start-col right-end-row right-end-col {})]
    (nvim.buf_set_text buf right-start-row right-start-col right-end-row right-end-col left-text)
    (nvim.buf_set_text buf left-start-row left-start-col left-end-row left-end-col right-text)
    (let [(row col) (if forward
                        (values right-start-row (- right-end-col (- left-end-col left-start-col)))
                        (values left-start-row left-start-col))]
      (nvim.win_set_cursor 0 [(+ row 1) col]))))

(fn *swap [buf parser query forward count]
  (let [capture-map (collect [k v (pairs query.captures)] (values v k))
        [tree] (parser:parse)
        [row col] (nvim.win_get_cursor 0)
        row (- row 1)]
    (var parent nil)
    (each [_ matches (query:iter_matches (tree:root) buf) &until parent]
      (match (. matches capture-map.parent)
        node (when (vim.treesitter.is_in_node_range node row col)
               (set parent node))))
    (when parent
      ; Index into the nodes array of the current node under the cursor
      (var k nil)
      (var done? false)
      (for [_ 1 count &until done?]
        (local nodes (icollect [id node (query:iter_captures parent buf)]
                       (if (= id capture-map.swap) node)))
        (when (not k)
          (each [i v (ipairs nodes) &until k]
            (when (vim.treesitter.is_in_node_range v row col)
              (set k i))))

        (let [cur (. nodes k)]
          (set k (if forward (+ k 1) (- k 1)))
          (case (. nodes k)
            other (swap-nodes buf forward (if forward (values cur other) (values other cur)))
            nil (set done? true)))))))

(fn swap [forward]
  (let [buf (nvim.get_current_buf)
        ft (. vim.bo buf :filetype)
        lang (or (vim.treesitter.language.get_lang ft) ft)
        query (vim.treesitter.query.get lang :swap)]
    (if query
        (case (vim.treesitter.get_parser bufnr lang {:error false})
          parser (*swap buf parser query forward vim.v.count1)
          nil (nvim.err_writeln (: "No parser found for language %s" :format lang)))
        (nvim.err_writeln (: "No 'swap' query found for language %s" :format lang))))
  (pcall vim.fn.repeat#set (if forward ">a" "<a") vim.v.count))

(keymap :n ">a" #(swap true))
(keymap :n "<a" #(swap false))
