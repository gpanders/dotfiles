(local disabled {})
(local ft-lang-map {})

(autocmd treesitter#highlight :FileType
  (fn [{: buf}]
    (let [ft (. vim.bo buf :filetype)]
      (when (not (. disabled ft))
        (case (. ft-lang-map ft)
          lang (do
                 (vim.treesitter.register lang ft)
                 (tset ft-lang-map ft nil)))
        ; TODO: Replace _get_parser with get_parser in 0.12
        (case (vim.treesitter._get_parser buf)
          nil (tset disabled ft true)
          parser (do
                   (when (not (. vim.treesitter.highlighter.active buf))
                     (vim.treesitter.highlighter.new parser))
                   (when (vim.treesitter.query.get ft :folds)
                     (set vim.wo.foldmethod :expr)
                     (set vim.wo.foldexpr "v:lua.vim.treesitter.foldexpr()"))))))))
