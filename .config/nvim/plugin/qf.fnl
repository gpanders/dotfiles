(keymap :n "yoq" #(let [{: winid : items} (vim.fn.getqflist {:winid 1 :items 1})]
                    (match (values winid (length items))
                      (0 0) nil
                      (0 n) (exec (: "copen %d|wincmd p" :format (math.min 10 n)))
                      _ (exec :cclose))) {:desc "Toggle quickfix list"})
(keymap :n "yol" #(let [{: winid : items} (vim.fn.getloclist 0 {:winid 1 :items 1})]
                    (match (values winid (length items))
                      (0 0) nil
                      (0 n) (exec (: "lopen %d|wincmd p" :format (math.min 10 n)))
                      _ (exec :lclose))) {:desc "Toggle location list"})

(autocmd qf# :QuickFixCmdPost
  #(match vim.g.quickfix_path_map
     map (let [list (vim.fn.getqflist {:items true :title true :winid true})
               cursor (nvim.win_get_cursor list.winid)
               delete {}]
           (each [_ v (ipairs list.items)]
             (when (= v.valid 1)
               (let [bufname (vim.fn.bufname v.bufnr)]
                 (var done? false)
                 (each [src dst (pairs map) &until done?]
                   (let [(fullpath n) (bufname:gsub src dst)]
                     (when (< 0 n)
                       (set done? true)
                       (tset delete v.bufnr true)
                       (set v.filename fullpath)
                       (set v.bufnr nil)))))))
           (each [bufnr (pairs delete)]
             (vim.api.nvim_buf_delete bufnr {}))
           (vim.fn.setqflist [] :r list)
           (nvim.win_set_cursor list.winid cursor))))
