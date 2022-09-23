(with-module [snippy :snippy]
  (snippy.setup {:mappings {:is {:<Tab> :expand_or_advance
                                 :<S-Tab> :previous}}
                 :scopes {:lua (fn [scopes]
                                 (let [path (vim.fn.fnamemodify (nvim.buf_get_name 0) :p)]
                                   (when (or (path:find (vim.fn.stdpath :config))
                                             (path:find (vim.fn.stdpath :data)))
                                     (table.insert scopes :nvim))
                                   scopes))}})
  (autocmd snippy# :CompleteDone snippy.complete_done))
