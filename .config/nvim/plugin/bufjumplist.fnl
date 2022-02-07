(macro t [s]
  `(vim.api.nvim_replace_termcodes ,s true true true))

(fn jump [forward?]
  (let [bufnr (vim.api.nvim_get_current_buf)
        [jumplist index] (vim.fn.getjumplist)
        (start stop step) (if forward?
                              (values (+ index 2) (length jumplist) 1)
                              (values index 1 -1))]
    (var target nil)
    (var count vim.v.count1)
    (for [i start stop step :until (= count 0)]
      (match (. jumplist i)
        {: bufnr} (do
                    (set count (- count 1))
                    (set target i))))
    (when target
      (let [cmd (.. "normal! "
                    (if forward?
                        (.. (+ 1 (- target start)) (t "<C-I>"))
                        (.. (+ 1 (- start target)) (t "<C-O>"))))]
        (exec cmd)))))

(keymap :n "[j" #(jump false))
(keymap :n "]j" #(jump true))
