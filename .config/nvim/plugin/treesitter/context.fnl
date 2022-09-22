(local state {})

(local lang-has-parser
       (let [lut {}]
         (fn [lang]
           (match (. lut lang)
             nil (let [has-parser (vim.treesitter.language.require_language lang nil true)]
                   (tset lut lang has-parser)
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

(fn node-at-cursor []
  (let [bufnr (nvim.get_current_buf)]
    (match (root bufnr)
      root-node (let [[lnum col] (nvim.win_get_cursor 0)
                      lnum (- lnum 1)]
                  (root-node:named_descendant_for_range lnum col lnum col)))))

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
        captures (collect [k v (pairs query.captures)] (values v k))
        (start-row start-col) (node:start)
        (end-row end-col) (do
                            (var end-node nil)
                            (each [pat mat (query:iter_matches node bufnr) :until end-node]
                              (match (. mat captures.end)
                                end (let [nod (. mat captures.context)]
                                      (match (nod:start)
                                        (start-row start-col) (set end-node end)))))
                            (when end-node
                              (end-node:end_)))]
    (-> (nvim.buf_get_text bufnr start-row 0 (or end-row (+ start-row 1)) (or end-col 0) {})
        (table.concat " ")
        (string.gsub "(%S)%s+" "%1 ")
        (string.gsub "%s*$" "")
        (->> (pick-values 1)))))

(fn close []
  (match state.winid
    w (do
        (nvim.win_close w true)
        (set state.winid nil))))

(fn show-context [bufnr]
  (match (context bufnr)
    contexts (let [lang (. vim.bo bufnr :filetype)
                   win (nvim.get_current_win)
                   [{: textoff : topline}] (vim.fn.getwininfo win)
                   width (- (nvim.win_get_width win) textoff)]
               (var text [])
               (each [_ ctx (ipairs contexts)]
                 (let [start-row (ctx:start)]
                   (if (< start-row (+ (- topline 1) (length text)))
                       (table.insert text (context-text bufnr ctx)))))
               (if (< 0 (length text))
                   (let [b (match (?. state bufnr :bufnr)
                             (where n (vim.api.nvim_buf_is_valid n)) n
                             _ (let [b (vim.api.nvim_create_buf false true)]
                                 (tset vim.bo b :buftype :nofile)
                                 (tset vim.bo b :readonly true)
                                 (tset vim.bo b :syntax (. vim.bo bufnr :filetype))
                                 (when (not (. state bufnr))
                                   (tset state bufnr {}))
                                 (tset state bufnr :bufnr b)
                                 b))
                         w (match state.winid
                             (where n (nvim.win_is_valid n)) (do
                                                               (nvim.win_set_config n {:relative :win
                                                                                       :row 0
                                                                                       :col textoff
                                                                                       :height (length text)
                                                                                       : width})
                                                               n)
                             _ (let [w (nvim.open_win b false {:relative :win
                                                               :win win
                                                               :row 0
                                                               :col textoff
                                                               : width
                                                               :height (length text)
                                                               :focusable false
                                                               :style :minimal
                                                               :noautocmd true})]
                                 (tset vim.wo w :winhighlight "NormalFloat:TreesitterContext")
                                 (set state.winid w)
                                 w))]
                     (nvim.win_set_buf w b)
                     (nvim.buf_set_lines b 0 -1 true text))
                   (close)))
    _ (close)))

(fn toc []
  (let [buf (nvim.get_current_buf)
        lang (. vim.bo buf :filetype)]
    (match (vim.treesitter.query.get_query lang :context)
      query
      (let [root-node (root buf)
            items []
            [context-id] (icollect [i v (ipairs query.captures)]
                           (if (= v :context) i))]
        (each [id subnode (query:iter_captures root-node)]
          (when (= id context-id)
            (let [(lnum _ end-lnum _) (subnode:range)]
              (table.insert items {:text (context-text buf subnode query)
                                   :bufnr buf
                                   :lnum (+ lnum 1)
                                   :end_lnum (+ end-lnum 1)}))))
        (vim.fn.setloclist 0 [] " " {: items
                                     :title (.. "Contexts in " (-> (nvim.buf_get_name buf)
                                                                   (vim.fn.fnamemodify ":.")))})
        (vim.cmd.lopen)))))


(augroup context#
  (autocmd [:WinScrolled :WinEnter :CursorMoved :CursorMovedI] "*"
    #(match (vim.fn.getcmdwintype)
       "" (let [buf (nvim.get_current_buf)
                lang (. vim.bo buf :filetype)]
            (if (lang-has-parser lang)
                (show-context buf)
                (close)))))
  (autocmd :FileType
    (fn [{: buf}]
      (let [lang (. vim.bo buf :filetype)]
        (when (lang-has-parser lang)
          (keymap :n "gO" toc {:buffer buf}))))))
