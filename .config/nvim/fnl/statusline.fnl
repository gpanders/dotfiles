(fn git []
  (match vim.b.gitsigns_head
    v v
    nil (match (pcall vim.fn.FugitiveHead)
          (true "") ""
          (true branch) (.. branch " ")
          _ "")))

(fn obsession []
  (match (pcall vim.fn.ObsessionStatus)
    (true "") ""
    (true s) (.. s " ")
    _ ""))

(fn lsp []
  (match vim.b.lsp
    {: name : client_id} (do
                           (var percentage nil)
                           (let [messages (icollect [_ v (ipairs (vim.lsp.util.get_progress_messages))]
                                            (do
                                              (when v.percentage
                                                (set percentage (math.max (or percentage 0) v.percentage)))
                                              (if v.message
                                                  (: "%s: %s" :format v.title v.message)
                                                  v.title)))
                                 msg-str (if percentage
                                             (: "%s [%02d]" :format (table.concat messages ", ") percentage)
                                             (table.concat messages ", "))]
                             (match (length msg-str)
                               0 (: " (%s/%d)" :format name client_id)
                               _ (: " (%s/%d %s)" :format name client_id msg-str))))
    _ ""))

(fn dap []
  (match vim.b.dap
    true (with-module [dap* :dap]
           (dap*.status))
    _ ""))

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
      (e w) (: "E: %d, W: %d " :format e w))))

{: git
 : lsp
 : dap
 : obsession
 : diagnostics}
