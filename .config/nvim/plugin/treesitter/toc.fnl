(local {: root : commands : context-text : lang-has-parser} (require :treesitter))

(fn commands.toc []
  (let [buf nvim.current.buf
        lang (. vim.bo buf.id :filetype)]
    (match (vim.treesitter.query.get_query lang :context)
      query
      (let [root-node (root buf.id)
            items []
            [context-id] (icollect [i v (ipairs query.captures)]
                           (if (= v :context) i))]
        (each [id subnode (query:iter_captures root-node)]
          (when (= id context-id)
            (let [(lnum _ end-lnum _) (subnode:range)]
              (table.insert items {:text (context-text buf.id subnode query)
                                   :bufnr buf.id
                                   :lnum (+ lnum 1)
                                   :end_lnum (+ end-lnum 1)}))))
        (vim.fn.setloclist 0 [] " " {: items
                                     :title (.. "Contexts in " (-> (buf:get_name)
                                                                   (vim.fn.fnamemodify ":.")))})
        (vim.cmd "lopen")))))

(autocmd treesitter#toc :FileType
  (fn [{: buf}]
    (let [lang (. vim.bo buf :filetype)]
      (when (lang-has-parser lang)
        (keymap :n "gO" commands.toc {:buffer buf})))))
