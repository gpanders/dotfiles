(keymap :i "<C-]>"
  #(let [snippy (require :snippy)]
     (if (snippy.can_expand)
         "<Plug>(snippy-expand)"
         "<C-]>"))
  {:expr true})

(keymap [:i :s] "<C-j>"
  #(let [snippy (require :snippy)]
     (if (snippy.can_jump 1)
         "<Plug>(snippy-next)"
         "<C-j>"))
  {:expr true})

(keymap [:i :s] "<C-k>"
  #(let [snippy (require :snippy)]
     (if (snippy.can_jump -1)
         "<Plug>(snippy-previous)"
         "<C-k>"))
  {:expr true})
