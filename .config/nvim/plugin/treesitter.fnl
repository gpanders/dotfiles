(local disabled {})
(local ft-lang-map {:terraform :hcl})

(autocmd treesitter#highlight :FileType
  (fn [args]
    (let [ft (. vim.bo args.buf :filetype)
          lang (or (. ft-lang-map ft) ft)]
      (when (not (. disabled ft))
        (match (pcall vim.treesitter.get_parser args.buf lang)
          false (tset disabled ft true)
          (true parser) (do
                          (require :nvim-treesitter.query_predicates)
                          (vim.treesitter.highlighter.new parser)
                          (set vim.wo.foldmethod :expr)
                          (set vim.wo.foldexpr "v:lua.vim.treesitter.foldexpr()")))))))
