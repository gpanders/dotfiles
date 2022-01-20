(set vim.g.zig_ast_check_autosave 0)
(set vim.g.zig_fmt_autosave 0)

(fn equal? [a b]
  (var eql? (= (length a) (length b)))
  (each [i v (ipairs a) :until (not eql?)]
    (set eql? (= v (. b i))))
  eql?)

(autocmd ft/zig :BufWritePre "<buffer>"
  (let [bufnr (vim.api.nvim_get_current_buf)
        text (vim.api.nvim_buf_get_lines bufnr 0 -1 true)
        out (vim.fn.systemlist "zig fmt --stdin" (table.concat text "\n"))]
    (when (and (= 0 vim.v.shell_error) (not (equal? text out)))
      (vim.api.nvim_buf_set_lines bufnr 0 -1 true out))))
