(local {: node-at-cursor : context : context-text : lang-has-parser} (require :treesitter))

(local state {})

(fn close []
  (match state.winid
    w (do
        (nvim.win.close w true)
        (set state.winid nil))))

(fn show-context [bufnr]
  (match (context bufnr)
    contexts (let [lines []
                   lang (. vim.bo bufnr :filetype)
                   win nvim.current.win
                   [{: textoff : topline}] (vim.fn.getwininfo win.id)
                   width (- (win:get_width) textoff)]
               (each [_ ctx (ipairs contexts)]
                 (let [start-row (ctx:start)]
                   (if (< start-row (- topline 1))
                       (table.insert lines (context-text bufnr ctx)))))
               (if (< 0 (length lines))
                   (let [b (match (?. state bufnr :bufnr)
                             (where n (vim.api.nvim_buf_is_valid n)) n
                             _ (let [b (vim.api.nvim_create_buf false true)]
                                 (tset vim.bo b :readonly true)
                                 (tset vim.bo b :filetype (. vim.bo bufnr :filetype))
                                 (when (not (. state bufnr))
                                   (tset state bufnr {}))
                                 (tset state bufnr :bufnr b)
                                 b))
                         w (match state.winid
                             (where n (vim.api.nvim_win_is_valid n)) (do
                                                                       (vim.api.nvim_win_set_config n {:relative :win
                                                                                                       :row 0
                                                                                                       :col textoff
                                                                                                       : width})
                                                                       n)
                             _ (let [w (vim.api.nvim_open_win b false {:relative :win
                                                                       :win win.id
                                                                       :row 0
                                                                       :col textoff
                                                                       : width
                                                                       :height 1
                                                                       :focusable false
                                                                       :style :minimal
                                                                       :noautocmd true})]
                                 (tset vim.wo w :winhighlight "NormalFloat:TreesitterContext")
                                 (set state.winid w)
                                 w))
                         lines (if (< 1 (length lines))
                                   (icollect [_ line (ipairs lines)]
                                     (-> line
                                         (vim.trim)
                                         (string.gsub "%s*[%[%(%{]*%s*$" "")
                                         (->> (pick-values 1))))
                                   lines)]
                     (nvim.win.set_buf w b)
                     (nvim.buf.set_lines b 0 -1 true [(table.concat lines " -> ")]))
                   (close)))
    _ (close)))

(autocmd treesitter# [:WinScrolled :CursorMoved :CursorMovedI]
  (let [bufnr (tonumber (vim.fn.expand "<abuf>"))
        lang (. vim.bo bufnr :filetype)]
    (if (lang-has-parser lang)
        (show-context bufnr)
        (close))))
