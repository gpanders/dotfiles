(local {: node-at-cursor : context : lang-has-parser} (require :treesitter))

(local state {})

(fn close []
  (match state.winid
    w (do
        (vim.api.nvim_win_close w true)
        (set state.winid nil))))

(fn show-context [bufnr]
  (match (context bufnr)
    contexts (let [lines []
                   winid (vim.api.nvim_get_current_win)
                   [{: textoff : topline}] (vim.fn.getwininfo winid)
                   width (- (vim.api.nvim_win_get_width winid) textoff)]
               (each [_ ctx (ipairs contexts)]
                 (let [start-row (ctx:start)]
                   (if (< (+ start-row 1) topline)
                       (let [[text] (vim.api.nvim_buf_get_lines bufnr start-row (+ start-row 1) true)]
                         (table.insert lines text)))))
               (if (< 0 (length lines))
                   (let [b (match (?. state bufnr :bufnr)
                             nil (let [b (vim.api.nvim_create_buf false true)]
                                   (tset vim.bo b :readonly true)
                                   (tset vim.bo b :filetype (. vim.bo bufnr :filetype))
                                   (when (not (. state bufnr))
                                     (tset state bufnr {}))
                                   (tset state bufnr :bufnr b)
                                   b)
                             n n)
                         w (match state.winid
                             nil (let [w (vim.api.nvim_open_win b false {:relative :win
                                                                         :win winid
                                                                         :row 0
                                                                         :col textoff
                                                                         : width
                                                                         :height 1
                                                                         :focusable false
                                                                         :style :minimal
                                                                         :noautocmd true})]
                                   (tset vim.wo w :winhighlight "NormalFloat:TreesitterContext")
                                   (set state.winid w)
                                   w)
                             n (do
                                 (vim.api.nvim_win_set_config n {:relative :win
                                                                 :row 0
                                                                 :col textoff
                                                                 : width})
                                 n))
                         lines (if (< 1 (length lines))
                                   (icollect [_ line (ipairs lines)]
                                     (-> line
                                         (vim.trim)
                                         (string.gsub "%s*[%[%(%{]*%s*$" "")
                                         (->> (pick-values 1))))
                                   lines)]
                     (vim.api.nvim_win_set_buf w b)
                     (vim.api.nvim_buf_set_lines b 0 -1 true [(table.concat lines " -> ")]))
                   (close)))
    _ (close)))

(autocmd treesitter# [:WinScrolled :CursorMoved :CursorMovedI]
  (let [bufnr (tonumber (vim.fn.expand "<abuf>"))
        lang (. vim.bo bufnr :filetype)]
    (if (lang-has-parser lang)
        (show-context bufnr)
        (close))))
