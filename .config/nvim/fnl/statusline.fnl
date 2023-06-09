(fn obsession []
  (match (pcall vim.fn.ObsessionStatus :$ :S)
    (true "") ""
    (true s) (.. " " s)
    _ ""))

(fn lsp-progress [client]
  (var percentage nil)
  (let [groups {}
        messages []]
    (each [{: token : value} client.progress]
      (when (?. value :kind)
        (let [group (case groups.token
                      group group
                      nil (let [group {}]
                            (set groups.token group)
                            group))]
          (set group.title (or value.title group.title))
          (set group.message (or value.message group.message))
          (when value.percentage
            (set percentage (math.max (or percentage 0) value.percentage))))))
    (each [_ group (pairs groups)]
      (let [m (if group.title
                  (if group.message
                      (: "%s: %s" :format group.title group.message)
                      group.title)
                  group.message)]
        (tset messages (+ (length messages) 1) m)))
    (let [message (table.concat messages ", ")]
      (if percentage
          (: "%s (%%%%%d)" :format message percentage)
          message))))

(fn lsp []
  (match-try vim.b.lsp
    name (vim.lsp.get_active_clients {:bufnr 0 : name})
    [client] (lsp-progress client)
    message (: " %s " :format message)
    (catch
      _ "")))

(fn diagnostics []
  (let [diags (vim.diagnostic.get 0 {:severity {:min vim.diagnostic.severity.WARN}})
        num-errors (accumulate [sum 0 _ v (ipairs diags)]
                     (if (= v.severity vim.diagnostic.severity.ERROR)
                         (+ sum 1)
                         sum))
        num-warnings (- (length diags) num-errors)]
    (match (values num-errors num-warnings)
      (0 0) ""
      (e 0) (: "E: %d " :format e)
      (0 w) (: "W: %d " :format w)
      (e w) (: "E: %d W: %d " :format e w))))

(fn filename [buf curwin]
  (let [name (match (nvim.buf_get_name buf)
               "" "Untitled"
               n (n:gsub "%%" "%%%%"))
        fname (vim.fn.fnamemodify name ":~:.")
              parent (fname:match "^(.*/)")
              tail (vim.fn.fnamemodify name ":t")
              (parent-hl tail-hl) (if vim.bo.modified
                                      (values "%1*" "%1*")
                                      (not curwin)
                                      (values "" "")
                                      (values "%2*" "%3*"))]
    (: "%s %%<%s%s%s %%*" :format parent-hl (or parent "")
                                  tail-hl tail)))

(fn tabline []
  (let [tabpagenr (vim.fn.tabpagenr)
        items []]
    (for [i 1 (vim.fn.tabpagenr :$)]
      (let [hi (if (= i tabpagenr) :TabLineSel :TabLine)
            cwd (-> (vim.fn.getcwd -1 i)
                    (vim.fn.fnamemodify ":~")
                    (vim.fn.pathshorten))]
        (table.insert items (: "%%#%s#%%%dT %d %s " :format hi i i cwd))))
    (table.insert items "%#TabLineFill#%T")
    (table.concat items)))

(fn statusline []
  (let [buf (nvim.get_current_buf)
        win (nvim.get_current_win)
        curwin (= (tonumber vim.g.actual_curwin) win)
        items [(obsession)
               (filename buf curwin)
               (if vim.bo.readonly
                   "%r "
                   "")
               (if vim.wo.previewwindow
                   "%w "
                   "")
               "%="
               (lsp)
               (diagnostics)
               (if curwin "%4*" "")
               (if (and vim.bo.modifiable (not vim.bo.readonly))
                   (let [t []]
                     (match vim.bo.fileformat
                       "unix" nil
                       ff (table.insert t (if (= ff :dos) "CRLF" "CR")))
                     (match vim.bo.fileencoding
                       "utf-8" nil
                       "" nil
                       fenc (table.insert t fenc))
                     (if (< 0 (length t))
                         (: " %s " :format (table.concat t " "))
                         ""))
                   "")
               (if curwin "%8*" "")
               (match vim.bo.filetype
                 "" ""
                 ft (match vim.b.lsp
                      name (: " %s/%s " :format ft name)
                      _ (: " %s " :format ft)))
               (if curwin "%9*" "")
               " %l:%c %P "]]
    (table.concat items)))

{: statusline
 : tabline}
