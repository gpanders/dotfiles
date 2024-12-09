(autocmd template# :BufNewFile "*"
  (fn [args]
    (let [fname (vim.fs.basename args.file)
          ext (vim.fn.fnamemodify args.file ":e")
          ft (. vim.bo args.buf :filetype)
          candidates [fname ext ft]]
      (var done? false)
      (each [_ candidate (ipairs candidates) &until done?]
        (let [tmpl (vim.fs.joinpath (vim.fn.stdpath :config) :templates (: "%s.tmpl" :format candidate))
              f (io.open tmpl :r)]
          (when f
            (vim.snippet.expand (f:read "*a"))
            (set done? true)))))))
