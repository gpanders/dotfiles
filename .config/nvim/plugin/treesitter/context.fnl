(local {: node-at-cursor : context} (require :treesitter))

(local state (setmetatable {} {:__index (fn [t k]
                                          (tset t k {})
                                          (. t k))}))

(fn close [bufnr]
  (when (?. state bufnr :winid)
    (vim.api.nvim_win_close (. state bufnr :winid) true)
    (tset state bufnr :winid nil)))

(fn show-context []
  (let [bufnr (vim.api.nvim_get_current_buf)
        winid (vim.api.nvim_get_current_win)
        [{: textoff}] (vim.fn.getwininfo winid)
        width (- (vim.api.nvim_win_get_width winid) textoff)]
    (match (context bufnr)
      contexts (let [lines []]
                 (each [_ ctx (ipairs contexts)]
                   (let [start-row (ctx:start)]
                     (match (vim.fn.screenpos winid (+ start-row 1) 1)
                       {:row 0} (let [[text] (vim.api.nvim_buf_get_lines bufnr start-row (+ start-row 1) true)]
                                  (table.insert lines text)))))
                 (if (< 0 (length lines))
                     (let [b (match (?. state bufnr :bufnr)
                               nil (let [b (vim.api.nvim_create_buf false true)]
                                     (tset vim.bo b :readonly true)
                                     (tset vim.bo b :filetype (. vim.bo bufnr :filetype))
                                     (tset state bufnr :bufnr b)
                                     b)
                               n n)
                           w (match (?. state bufnr :winid)
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
                                     (tset state bufnr :winid w)
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
                       (vim.api.nvim_buf_set_lines b 0 -1 true [(table.concat lines " -> ")]))
                     (close bufnr)))
      _ (close bufnr))))

(autocmd treesitter# :FileType "*"
  (let [bufnr (tonumber (vim.fn.expand "<abuf>"))
        lang (. vim.bo bufnr :filetype)]
    (when (vim.treesitter.language.require_language lang nil true)
      (autocmd treesitter# [:WinScrolled :CursorMoved :CursorMovedI] "<buffer=abuf>" (show-context)))))
