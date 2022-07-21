(keymap :n "<Space>q" #(let [{: winid : items} (vim.fn.getqflist {:winid 1 :items 1})]
                         (match (values winid (length items))
                           (0 0) nil
                           (0 n) (vim.cmd (: "copen %d|wincmd p" :format (math.min 10 n)))
                           _ (vim.cmd.cclose))))
(keymap :n "<Space>l" #(let [{: winid : items} (vim.fn.getloclist 0 {:winid 1 :items 1})]
                         (match (values winid (length items))
                           (0 0) nil
                           (0 n) (vim.cmd (: "lopen %d|wincmd p" :format (math.min 10 n)))
                           _ (vim.cmd.lclose))))

(fn list [prefix dir]
  (match (pcall vim.cmd (.. vim.v.count1 prefix dir))
    (false err) (when (err:find "E553")
                  (vim.cmd (.. prefix (if (= dir :next) :first :last))))))

(keymap :n "[l" #(list :l :prev))
(keymap :n "]l" #(list :l :next))
(keymap :n "[q" #(list :c :prev))
(keymap :n "]q" #(list :c :next))
(keymap :n "[L" #(vim.cmd.lfirst))
(keymap :n "]L" #(vim.cmd.llast))
(keymap :n "[Q" #(vim.cmd.cfirst))
(keymap :n "]Q" #(vim.cmd.clast))
(keymap :n "[<C-L>" #(vim.cmd {:count vim.v.count1 :cmd "lolder"}))
(keymap :n "]<C-L>" #(vim.cmd {:count vim.v.count1 :cmd "lnewer"}))
(keymap :n "[<C-Q>" #(vim.cmd {:count vim.v.count1 :cmd "colder"}))
(keymap :n "]<C-Q>" #(vim.cmd {:count vim.v.count1 :cmd "cnewer"}))
