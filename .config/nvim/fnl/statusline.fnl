(fn obsession []
  (match (pcall vim.fn.ObsessionStatus)
    (true "") ""
    (true s) (.. s " ")
    _ ""))

(fn lsp-progress-messages []
  (let [messages []]
    (each [client-id client (pairs (vim.lsp.buf_get_clients))]
      (each [k v (pairs client.messages.progress)]
        (when (not v.done)
          (let [msg (if v.message
                        (: "%s: %s" :format v.title v.message)
                        v.title)]
            (table.insert messages (if v.percentage
                                       (: "%s (%%%%%d)" :format msg v.percentage)
                                       msg)))))
      (each [_ v (ipairs client.messages.messages)]
        (when (and v.show_once (= v.shown 0))
          (table.insert messages v.content))
        (set v.shown (+ v.shown 1)))
      (each [k v (pairs client.messages.status)]
        (assert false (.. k ": " (vim.inspect v)))))
    messages))

(fn lsp []
  (let [clients (icollect [client-id client (pairs (vim.lsp.buf_get_clients))]
                  (: "%s/%d" :format client.config.name client-id))]
    (match (length clients)
      0 ""
      _ (let [clients (table.concat clients ", ")
              messages (lsp-progress-messages)]
          (match (length messages)
            0 (: "(%s) " :format clients)
            _ (: "(%s) %s " :format clients (table.concat messages ", ")))))))


(fn diagnostics []
  (let [diags (vim.diagnostic.get 0 {:severity {:min vim.diagnostic.severity.WARN}})
        num-errors (accumulate [sum 0 _ v (ipairs diags)]
                     (if (= v.severity vim.diagnostic.severity.ERROR)
                         (+ sum 1)
                         sum))
        num-warnings (- (length diags) num-errors)]
    (match (values num-errors num-warnings)
      (0 0) ""
      (e 0) (: "E:%-4d" :format e)
      (0 w) (: "W:%-4d" :format w)
      (e w) (: "E:%-4d W:%-4d" :format e w))))

(fn filename []
  (match (nvim.buf.get_name 0)
    "" "[No Name]"
    n (vim.fn.fnamemodify n ":~:.")))

(fn []
  (let [items [" "
               (obsession)
               (if vim.bo.modified
                   "%2*"
                   "%1*")
               "%<"
               (filename)
               "%* "
               (if vim.bo.readonly
                   "%r "
                   "")
               (match vim.bo.filetype
                 "" ""
                 ft (: "[%s] " :format ft))
               (lsp)
               "%="
               (diagnostics)
               (if (and vim.bo.modifiable (not vim.bo.readonly))
                   (..
                     (match vim.bo.fileformat
                       "unix" ""
                       ff (: "[%s] " :format ff))
                     (match vim.bo.fileencoding
                       "utf-8" ""
                       "" ""
                       fenc (: "[%s] " :format fenc)))
                   "")
               "%10.(%l:%c%V%)%6.P"
               "  "]]
    (table.concat items)))
