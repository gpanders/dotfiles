(fn obsession []
  (match (pcall vim.fn.ObsessionStatus)
    (true "") ""
    (true s) (.. s " ")
    _ ""))

(fn lsp-progress-messages []
  (let [messages []]
    (each [client-id client (pairs (vim.lsp.buf_get_clients))]
      (each [k v (pairs client.messages.progress)]
        (let [msg (if v.message
                      (: "%s: %s" :format v.title v.message)
                      v.title)]
          (table.insert messages (if v.percentage
                                     (: "%s (%%%%%d)" :format msg v.percentage)
                                     msg)))
        (when v.done
          (tset client.messages.progress k nil))))
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
  (let [buf nvim.current.buf]
    (match (buf:get_name)
      "" "[No Name]"
      n (let [cwd (vim.loop.cwd)
              cwd (if (string.find n cwd)
                      (: "%s/" :format (vim.fn.fnamemodify cwd ":t"))
                      "")
              fname (vim.fn.fnamemodify n ":~:.")
              parent (fname:match "^(.*/)")
              tail (vim.fn.fnamemodify n ":t")
              (parent-hl tail-hl cwd-hl) (if vim.bo.modified
                                             (values "%1*" "%1*" "%1*")
                                             (not= (tonumber vim.g.actual_curbuf) buf.id)
                                             (values "" "%3*" "")
                                             (values "%2*" "%3*" "%4*"))]
          (: "%s%s%%<%s%s%s%s%%* " :format cwd-hl cwd
                                           parent-hl (or parent "")
                                           tail-hl tail)))))

(fn []
  (let [items [" "
               (obsession)
               (filename)
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
