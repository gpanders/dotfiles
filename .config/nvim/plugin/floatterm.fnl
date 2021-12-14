(local default-command :fish)

(fn create-term-buf [cmd]
  (let [bufnr (vim.api.nvim_create_buf true false)]
    (vim.api.nvim_buf_call bufnr #(vim.fn.termopen cmd))
    (vim.api.nvim_buf_set_var bufnr :floatbuf true)
    (vim.api.nvim_command
      (: "autocmd TermLeave <buffer=%d> call nvim_win_close(bufwinid(%d), 0)" :format bufnr bufnr))
    bufnr))

(fn open-term-win [cmd]
  (var bufnr nil)
  (each [_ b (ipairs (vim.api.nvim_list_bufs)) :until bufnr]
    (match (pcall vim.api.nvim_buf_get_var b :floatbuf)
      (where (true v) v) (set bufnr b)))
  (when (not bufnr)
    (set bufnr (create-term-buf cmd)))
  (let [winid (vim.api.nvim_open_win bufnr true {:relative :editor
                                                 :width vim.o.columns
                                                 :height 25
                                                 :row (- vim.o.lines 25)
                                                 :col 0
                                                 :border [["─" :Normal]
                                                          ["─" :Normal]
                                                          ["─" :Normal]
                                                          "" "" "" "" ""]})]
    (vim.api.nvim_win_set_option winid :winhighlight "NormalFloat:Normal")
    (set vim.o.showmode false)
    (vim.api.nvim_command
      (: "autocmd WinLeave <buffer=%d> ++once lua assert(false)" :format bufnr))
    (vim.api.nvim_command "startinsert")
    (vim.api.nvim_command "mode")
    winid))

(command :Term {:nargs "*"}
  (fn [_ _ ?args]
    (let [args (if (= "" ?args) default-command ?args)
          cmd (vim.split args " " {:trimempty true})]
      (open-term-win cmd))))

(keymap :n "<Bslash>t" "<Cmd>Term<CR>")
