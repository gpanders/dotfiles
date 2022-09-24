(autocmd snippy# :InsertEnter {:once true}
  #(do
     (vim.cmd "packadd nvim-snippy")
     (let [snippy (require :snippy)]
       (snippy.setup {:mappings {:is {:<Tab> :expand_or_advance
                                      :<S-Tab> :previous}}})
       (autocmd snippy# :CompleteDone snippy.complete_done))))
