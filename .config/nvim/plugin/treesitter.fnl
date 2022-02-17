(local {: node-at-cursor : highlight-node} (require :treesitter))

(local ns (vim.api.nvim_create_namespace ""))

(fn ts-cursor []
  (let [bufnr (vim.api.nvim_get_current_buf)
        node (node-at-cursor)]
    (highlight-node bufnr ns node)
    (autocmd :CursorMoved "<buffer>" :once (vim.api.nvim_buf_clear_namespace bufnr ns 0 -1))
    (print (node:sexpr))))

(command :TSCursor {} ts-cursor)
