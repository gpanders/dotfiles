;; This plugin shows the "context" of the cursor position in a buffer. The
;; context is usually (but not necessarily) a function. If the beginning of the
;; current context is not visible in the window, a floating window is created
;; at the top of the current window that shows the current context.
;;
;; The context is language dependent and is defined by a "context" query. The
;; text shown in the floating window is the first line of the context, unless a
;; @text capture is used, in which case the text shown is from the beginning of
;; the context until the node specified by the @text capture.
;;
;; This plugin also provides a table of contents (TOC) function. This shows all
;; of the contexts within a buffer in the location list. The text for each TOC
;; entry is the same as the text in the floating window, unless the context
;; query uses a @toc capture, in which case the text is the beginning of the
;; context until the node specified by the @toc capture.

(local state {})

(local filetype-has-parser
       (let [lut {}]
         (fn [ft]
           (match (. lut ft)
             nil (let [lang (or (vim.treesitter.language.get_lang ft) ft)
                       has-parser (pcall vim.treesitter.language.add lang {:filetype ft})]
                   (tset lut ft has-parser)
                   has-parser)
             v v))))

(fn root [bufnr]
  (let [bufnr (or bufnr (nvim.get_current_buf))]
    (match (pcall vim.treesitter.get_parser bufnr)
      (true parser) (let [[tree] (parser:parse)]
                      (tree:root))
      _ nil)))

(fn parents [start]
  (values (fn [_ node] (when node (node:parent))) nil start))

(fn rev [l]
  (values (fn [s v]
            (when (< 1 v)
              (values (- v 1) (. s (- v 1)))))
          l
          (+ (length l) 1)))

(fn context [bufnr]
  (let [ft (. vim.bo bufnr :filetype)
        lang (or (vim.treesitter.language.get_lang ft) ft)
        query (vim.treesitter.query.get lang :context)]
    (when query
      (let [[row col] (nvim.win_get_cursor 0)
            row (- row 1)
            captures (collect [k v (pairs query.captures)] (values v k))
            curnode (vim.treesitter.get_node)
            contexts []]
        (each [parent (parents curnode)]
          (each [_ match# (query:iter_matches parent bufnr 0 -1 {:max_start_depth 0})]
            (match (. match# captures.context)
              node (tset contexts (+ (length contexts) 1) node))))
        (if (< 0 (length contexts))
            (icollect [_ v (rev contexts)] v))))))

(fn context-text [bufnr node ?query ?end-capture]
  (let [ft (. vim.bo bufnr :filetype)
        lang (or (vim.treesitter.language.get_lang ft) ft)
        query (or ?query (vim.treesitter.query.get lang :context))
        captures (collect [k v (pairs query.captures)] (values v k))
        (start-row start-col) (node:start)
        (end-row end-col) (do
                            (var end-node nil)
                            (each [_ mat (query:iter_matches node bufnr 0 -1 {:max_start_depth 0}) &until end-node]
                              (match (. mat (. captures (or ?end-capture :text)))
                                text (let [ctx (. mat captures.context)]
                                       (match (ctx:start)
                                         (start-row start-col) (set end-node text)))))
                            (when end-node
                              (end-node:end_)))
        text (if (and end-row end-col)
                 (nvim.buf_get_text bufnr start-row 0 end-row end-col {})
                 (nvim.buf_get_lines bufnr start-row (+ start-row 1) true))]
    (-> text
        (table.concat " ")
        (string.gsub "(%S)%s+" "%1 ")
        (string.gsub "%(%s+" "(")
        (string.gsub "%s+%)" ")")
        (string.gsub "%s*$" "")
        (->> (pick-values 1)))))

(fn close []
  (when (and state.winid (nvim.win_is_valid state.winid))
    (nvim.win_close state.winid true))
  (set state.winid nil))

(fn stale? [contexts width topline]
  (let [ctx (: (. contexts (length contexts)) :id)]
    (match state.last
      {: ctx : width : topline} false
      _ true)))

(fn show-context [bufnr]
  (match (context bufnr)
    nil (do
          (set state.last nil)
          (close))
    contexts (let [win (nvim.get_current_win)
                   [{: width : textoff : topline}] (vim.fn.getwininfo win)
                   width (- width textoff)]
               (when (stale? contexts width topline)
                 (set state.last {:ctx (: (. contexts (length contexts)) :id) : width : topline})

                 ; Array of [context, end] pairs. 'context' is the text to show
                 ; for the given context in the floating window. 'end' is the
                 ; end marker of the context (e.g. "}" for a function
                 ; declaration in C). These are included in the buffer to make
                 ; syntax highlighting work correctly.
                 (local texts [])
                 (each [_ ctx (ipairs contexts)]
                   (let [start-row (ctx:start)]
                     (when (< start-row (+ (- topline 1) (length texts)))
                       (var end-node ctx)
                       (while (and (end-node:named) (< 0 (end-node:child_count)))
                         (set end-node (end-node:child (- (end-node:child_count) 1))))
                       (tset texts (+ (length texts) 1) [(context-text bufnr ctx) (end-node:type)]))))

                 (local height (length texts))
                 (if (< 0 height)
                     (let [b (match (?. state bufnr :bufnr)
                               (where n (vim.api.nvim_buf_is_valid n)) n
                               _ (let [b (vim.api.nvim_create_buf false true)]
                                   (tset vim.bo b :buftype :nofile)
                                   (tset vim.bo b :readonly true)
                                   (tset vim.bo b :filetype (. vim.bo bufnr :filetype))
                                   (when (not (. state bufnr))
                                     (tset state bufnr {}))
                                   (tset state bufnr :bufnr b)
                                   b))
                           w (match state.winid
                               (where n (nvim.win_is_valid n)) (do
                                                                 (nvim.win_set_config n {:relative :win
                                                                                         :row 0
                                                                                         :col textoff
                                                                                         : height
                                                                                         : width})
                                                                 n)
                               _ (let [w (nvim.open_win b false {:relative :win
                                                                 :win win
                                                                 :row 0
                                                                 :col textoff
                                                                 : width
                                                                 : height
                                                                 :focusable false
                                                                 :style :minimal
                                                                 :noautocmd true})]
                                   (tset vim.wo w :winhighlight "NormalFloat:TreesitterContext")
                                   (tset vim.wo w :wrap false)
                                   (tset vim.wo w :listchars "extends:…")
                                   (set state.winid w)
                                   w))]
                       (nvim.win_set_buf w b)
                       (let [context-lines (icollect [_ v (ipairs texts)] (. v 1))
                             context-ends (icollect [_ v (rev texts)] (. v 2))]
                         (nvim.buf_set_lines b 0 -1 true context-lines)
                         (nvim.buf_set_lines b -1 -1 true context-ends)))
                     (close))))))

(fn toc []
  (let [buf (nvim.get_current_buf)
        lang (. vim.bo buf :filetype)]
    (match (vim.treesitter.query.get lang :context)
      query
      (let [root-node (root buf)
            [context-id] (icollect [i v (ipairs query.captures)]
                           (if (= v :context) i))
            items (icollect [id subnode (query:iter_captures root-node)]
                    (when (= id context-id)
                      (let [(lnum _ end-lnum _) (subnode:range)]
                        {:text (context-text buf subnode query :toc)
                         :bufnr buf
                         :lnum (+ lnum 1)
                         :end_lnum (+ end-lnum 1)})))]
        (vim.fn.setloclist 0 [] " " {: items
                                     :title (.. "Contexts in " (-> (nvim.buf_get_name buf)
                                                                   (vim.fn.fnamemodify ":.")))})
        (vim.cmd.lopen)))))


(augroup context#
  (autocmd [:WinScrolled :WinEnter :CursorMoved :CursorMovedI] "*"
    #(when (= "" (vim.fn.getcmdwintype))
       (let [buf (nvim.get_current_buf)
                ft (. vim.bo buf :filetype)]
         (if (filetype-has-parser ft)
             (show-context buf)
             (close)))))
  (autocmd :FileType
    (fn [{: buf}]
      (let [ft (. vim.bo buf :filetype)
            lang (or (vim.treesitter.language.get_lang ft) ft)]
        (when (filetype-has-parser ft)
          (keymap :n "gO" toc {:buffer buf}))))))
