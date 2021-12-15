(local default-command :fish)

(fn create-term-buf [cmd]
  (let [bufnr (vim.api.nvim_create_buf true false)]
    (vim.api.nvim_buf_call bufnr #(vim.fn.termopen cmd))
    (vim.api.nvim_buf_set_var bufnr :floatbuf true)
    bufnr))

(fn open-term-win [cmd]
  (var bufnr nil)
  (each [_ b (ipairs (vim.api.nvim_list_bufs)) :until bufnr]
    (match (pcall vim.api.nvim_buf_get_var b :floatbuf)
      (where (true v) v) (set bufnr b)))
  (when (not bufnr)
    (set bufnr (create-term-buf cmd)))
  (match (vim.fn.win_findbuf bufnr)
    [winid] (vim.api.nvim_set_current_win winid)
    _ (do
        (vim.api.nvim_command "botright 20new")
        (vim.api.nvim_win_set_buf 0 bufnr)))
  (vim.api.nvim_command "startinsert"))

(command :Term {:nargs "*"}
  (fn [_ _ ?args]
    (let [args (if (= "" ?args) default-command ?args)
          cmd (vim.split args " " {:trimempty true})]
      (open-term-win cmd))))

(keymap :n "<Bslash>t" "<Cmd>Term<CR>")
