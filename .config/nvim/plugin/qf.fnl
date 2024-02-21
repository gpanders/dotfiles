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

(fn list [prefix dir]
  (match (pcall vim.cmd (.. vim.v.count1 prefix dir))
    (false err) (when (err:find "E553")
                  (exec (.. prefix (if (= dir :next) :first :last))))))

(keymap :n "[l" #(list :l :prev))
(keymap :n "]l" #(list :l :next))
(keymap :n "[q" #(list :c :prev))
(keymap :n "]q" #(list :c :next))
(keymap :n "[L" #(exec :lfirst))
(keymap :n "]L" #(exec :llast))
(keymap :n "[Q" #(exec :cfirst))
(keymap :n "]Q" #(exec :clast))
(keymap :n "[<C-L>" #(vim.cmd {:count vim.v.count1 :cmd "lolder"}))
(keymap :n "]<C-L>" #(vim.cmd {:count vim.v.count1 :cmd "lnewer"}))
(keymap :n "[<C-Q>" #(vim.cmd {:count vim.v.count1 :cmd "colder"}))
(keymap :n "]<C-Q>" #(vim.cmd {:count vim.v.count1 :cmd "cnewer"}))

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
