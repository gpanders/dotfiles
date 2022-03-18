(local default-command :fish)

(fn create-term-buf [cmd ?tag]
  (let [bufnr (nvim.create_buf true false)]
    (nvim.buf.call bufnr #(vim.fn.termopen cmd))
    (when ?tag
      (nvim.buf.set_var bufnr ?tag true))
    (augroup term#
      (autocmd :BufWinLeave {:buffer bufnr}
        (match (. vim.b bufnr :winid)
          (where winid (nvim.win.is_valid winid)) (tset vim.b bufnr :winheight (nvim.win.get_height winid)))
        (tset vim.b bufnr :winid nil))
      (autocmd :BufWinEnter {:buffer bufnr}
        (let [winid nvim.current.win]
          (tset vim.b bufnr :winid winid)
          (match (. vim.b bufnr :winheight)
            height (nvim.win.set_height winid height)))))
    bufnr))

(fn open-term-win [cmd ?tag]
  (var bufnr nil)
  (when ?tag
    (each [_ b (ipairs (nvim.list_bufs)) :until bufnr]
      (match (pcall nvim.buf.get_var b ?tag)
        (true true) (set bufnr b))))
  (when (not bufnr)
    (set bufnr (create-term-buf cmd ?tag)))
  (match (. vim.b bufnr :winid)
    (where winid (nvim.win.is_valid winid)) (nvim.set_current_win winid)
    _ (do
        (nvim.command "botright new")
        (nvim.win.set_buf 0 bufnr)))
  (nvim.command "startinsert"))

(command :Term {:nargs "*"}
  (fn [{: args}]
    (let [args (if (= "" args) default-command args)
          cmd (vim.split args " " {})]
      (open-term-win cmd))))

(keymap :n "t<CR>" #(open-term-win default-command (: "term%d" :format vim.v.count)))

(exec "cnoreabbrev <expr> term     (getcmdtype() ==# ':' && getcmdline() ==# 'term') ? 'Term'  : 'term'")
