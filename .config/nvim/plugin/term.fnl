(local default-command :fish)

(fn create-term-buf [cmd ?tag]
  (let [bufnr (vim.api.nvim_create_buf true false)]
    (vim.api.nvim_buf_call bufnr #(vim.fn.termopen cmd))
    (when ?tag
      (vim.api.nvim_buf_set_var bufnr ?tag true))
    bufnr))

(fn open-term-win [cmd ?tag]
  (var bufnr nil)
  (when ?tag
    (each [_ b (ipairs (vim.api.nvim_list_bufs)) :until bufnr]
      (match (pcall vim.api.nvim_buf_get_var b ?tag)
        (where (true v) v) (set bufnr b))))
  (when (not bufnr)
    (set bufnr (create-term-buf cmd ?tag)))
  (match (vim.fn.win_findbuf bufnr)
    [winid] (vim.api.nvim_set_current_win winid)
    _ (do
        (vim.api.nvim_command "botright new")
        (vim.api.nvim_win_set_buf 0 bufnr)))
  (vim.api.nvim_command "startinsert"))

(command :Term {:nargs "*"}
  (fn [{: args}]
    (let [args (if (= "" args) default-command args)
          cmd (vim.split args " " {})]
      (open-term-win cmd))))

(keymap :n "t<CR>" #(open-term-win :fish (: "term%d" :format vim.v.count)))

(exec "cnoreabbrev <expr> term     (getcmdtype() ==# ':' && getcmdline() ==# 'term') ? 'Term'  : 'term'")
