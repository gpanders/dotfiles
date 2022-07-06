(local {: root : commands : context-text : lang-has-parser} (require :treesitter))

(fn commands.toc []
  (let [buf (nvim.get_current_buf)
        lang (. vim.bo buf :filetype)]
    (match (vim.treesitter.query.get_query lang :context)
      query
      (let [root-node (root buf)
            items []
            [context-id] (icollect [i v (ipairs query.captures)]
                           (if (= v :context) i))]
        (each [id subnode (query:iter_captures root-node)]
          (when (= id context-id)
            (let [(lnum _ end-lnum _) (subnode:range)]
              (table.insert items {:text (context-text buf subnode query)
                                   :bufnr buf
                                   :lnum (+ lnum 1)
                                   :end_lnum (+ end-lnum 1)}))))
        (vim.fn.setloclist 0 [] " " {: items
                                     :title (.. "Contexts in " (-> (nvim.buf_get_name buf)
                                                                   (vim.fn.fnamemodify ":.")))})
        (vim.cmd "lopen")))))

(autocmd treesitter#toc :FileType
  (fn [{: buf}]
    (let [lang (. vim.bo buf :filetype)]
      (when (lang-has-parser lang)
        (keymap :n "gO" commands.toc {:buffer buf})))))
