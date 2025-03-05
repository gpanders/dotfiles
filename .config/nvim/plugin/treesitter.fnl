(local disabled {})
(local ft-lang-map {})

(autocmd treesitter#highlight :FileType
  (fn [{: buf}]
    (when (vim.api.nvim_buf_is_loaded buf)
      (let [ft (. vim.bo buf :filetype)]
        (when (not (. disabled ft))
          (case (. ft-lang-map ft)
            lang (do
                   (vim.treesitter.language.register lang ft)
                   (tset ft-lang-map ft nil)))
          (case (vim.treesitter.get_parser buf nil {:error false})
            nil (tset disabled ft true)
            parser (do
                     (when (not (. vim.treesitter.highlighter.active buf))
                       (vim.treesitter.highlighter.new parser))
                     (when (vim.treesitter.query.get ft :folds)
                       (set vim.wo.foldmethod :expr)
                       (set vim.wo.foldexpr "v:lua.vim.treesitter.foldexpr()")))))))))
