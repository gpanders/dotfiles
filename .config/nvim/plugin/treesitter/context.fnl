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
                       has-parser (vim.treesitter.language.add lang {:silent true :filetype ft})]
                   (tset lut ft has-parser)
                   has-parser)
             v v))))

(fn root [bufnr]
  (let [bufnr (or bufnr (nvim.get_current_buf))]
    (match (pcall vim.treesitter.get_parser bufnr)
      (true parser) (let [[tree] (parser:parse)]
                      (tree:root))
      _ nil)))

(fn contains-node? [node other]
  (let [(start-row start-col end-row end-col) (node:range)
        (other-start-row other-start-col other-end-row other-end-col) (other:range)]
    (if (< start-row other-start-row)
        (or (< other-end-row end-row)
            (and (= end-row other-end-row) (<= other-end-col end-col)))
        (and (= start-row other-start-row) (<= other-end-col end-col)))))

(fn context [bufnr]
  (let [ft (. vim.bo bufnr :filetype)
        lang (or (vim.treesitter.language.get_lang ft) ft)
        query (vim.treesitter.get_query lang :context)]
    (when query
      (let [cursor-node (vim.treesitter.get_node)]
        (when cursor-node
          (let [scopes []
                (end-row end-col) (cursor-node:end_)
                captures (collect [k v (pairs query.captures)] (values v k))]
            (each [id ctx (query:iter_captures (root bufnr) bufnr 0 end-row)]
              (when (and (= id captures.context) (contains-node? ctx cursor-node))
                (table.insert scopes ctx)))
           scopes))))))

(fn context-text [bufnr node ?query ?end-capture]
  (let [ft (. vim.bo bufnr :filetype)
        lang (or (vim.treesitter.language.get_lang ft) ft)
        query (or ?query (vim.treesitter.get_query lang :context))
        captures (collect [k v (pairs query.captures)] (values v k))
        (start-row start-col) (node:start)
        (end-row end-col) (do
                            (var end-node nil)
                            (each [_ mat (query:iter_matches node bufnr) &until end-node]
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
  (match state.winid
    w (do
        (when (nvim.win_is_valid w)
          (nvim.win_close w true))
        (set state.winid nil))))

(fn show-context [bufnr]
  (match (context bufnr)
    nil (close)
    contexts (let [win (nvim.get_current_win)
                   [{: textoff : topline}] (vim.fn.getwininfo win)
                   width (- (nvim.win_get_width win) textoff)]
               (var text [])
               (each [_ ctx (ipairs contexts)]
                 (let [start-row (ctx:start)]
                   (if (< start-row (+ (- topline 1) (length text)))
                       (let [t (context-text bufnr ctx)]
                         (var end-node ctx)
                         (while (end-node:named)
                           (set end-node (end-node:child (- (end-node:child_count) 1))))
                         (let [end-text (end-node:type)]
                           (table.insert text (+ (/ (length text) 2) 1) (if (< (length t) width)
                                                                            t
                                                                            (: "%s ..." :format (t:sub 1 (- width 4)))))
                           (table.insert text (+ (/ (length text) 2) 2) end-text))))))
               (local height (/ (length text) 2))
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
                                 (tset vim.wo w :linebreak false)
                                 (set state.winid w)
                                 w))]
                     (nvim.win_set_buf w b)
                     (nvim.buf_set_lines b 0 -1 true text))
                   (close)))))

(fn toc []
  (let [buf (nvim.get_current_buf)
        lang (. vim.bo buf :filetype)]
    (match (vim.treesitter.query.get_query lang :context)
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
    #(match (vim.fn.getcmdwintype)
       "" (let [buf (nvim.get_current_buf)
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
