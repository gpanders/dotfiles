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
        (case (pcall vim.treesitter.get_parser buf)
          false (tset disabled ft true)
          (true parser) (do
                          (vim.treesitter.highlighter.new parser)
                          (set vim.wo.foldmethod :expr)
                          (set vim.wo.foldexpr "v:lua.vim.treesitter.foldexpr()")))))))
