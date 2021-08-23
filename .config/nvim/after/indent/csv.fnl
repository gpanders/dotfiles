(local {: max-column-width} (require "ft/csv"))

(setlocal noexpandtab)
(setlocal shiftwidth 0)

(append! vim.b.undo_indent "|setl et< sw<")

(let [first-line (. (vim.api.nvim_buf_get_lines 0 0 1 true) 1)]
  (when (and (first-line:find "\t") (not (first-line:find ",")))
    (let [tabstop (max-column-width (vim.api.nvim_buf_get_lines 0 0 -1 true))]
      (setlocal tabstop (+ tabstop 1))
      (append! vim.b.undo_indent "|setl ts<"))))
