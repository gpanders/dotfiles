(when vim.g.loaded_snippy
  (autocmd snippy :InsertEnter "*" :once
    (let [snippy (require :snippy)]
      (snippy.setup {:snippet_dirs (.. (vim.fn.stdpath :config) "/snippets")
                     :hl_group :Visual
                     :mappings {:is {:<Tab> :expand_or_advance
                                     :<S-Tab> :previous}
                                :x {:<Tab> :cut_text}}}))))
