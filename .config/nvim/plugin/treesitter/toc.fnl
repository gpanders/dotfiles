(local {: root : commands : context-text : lang-has-parser} (require :treesitter))

(fn commands.toc []
  (let [bufnr nvim.current.buf
        lang (. vim.bo bufnr :filetype)]
    (match (vim.treesitter.query.get_query lang :context)
      query
      (let [root-node (root bufnr)
            root-id (root-node:id)
            items []
            [context-id] (icollect [i v (ipairs query.captures)]
                           (if (= v :context) i))]
        (each [id subnode (query:iter_captures root-node)]
          (when (and (= id context-id) (= (: (subnode:parent) :id) root-id))
            (let [(lnum _ end-lnum _) (subnode:range)]
              (table.insert items {:text (context-text bufnr subnode query)
                                   : bufnr
                                   :lnum (+ lnum 1)
                                   :end_lnum (+ end-lnum 1)}))))
        (vim.fn.setloclist 0 [] " " {: items
                                     :title (.. "Contexts in " (-> (nvim.buf.get_name bufnr)
                                                                   (vim.fn.fnamemodify ":.")))})
        (exec "lopen")))))

(autocmd treesitter#toc :FileType
  (let [bufnr (tonumber (vim.fn.expand "<abuf>"))
        lang (. vim.bo bufnr :filetype)]
    (when (lang-has-parser lang)
      (keymap :n "gO" commands.toc {:buffer bufnr}))))
