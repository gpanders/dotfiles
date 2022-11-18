(autocmd snippy# :CompleteDone
  #(let [snippy (require :snippy)]
     (snippy.complete_done)))

(keymap [:i :s] "<Tab>" "<Plug>(snippy-expand-or-advance)")
(keymap [:i :s] "<S-Tab>" "<Plug>(snippy-previous)")
