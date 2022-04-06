(local default-command :fish)

(fn create-term-buf [cmd ?tag]
  (let [bufnr (nvim.create_buf true false)]
    (nvim.buf.call bufnr #(vim.fn.termopen cmd))
    (when ?tag
      (tset vim.b bufnr ?tag true))
    (augroup term#
      (autocmd :BufWinLeave {:buffer bufnr}
        "Remember height of buffer window"
        (match (. vim.b bufnr :winid)
          (where winid (nvim.win.is_valid winid)) (tset vim.b bufnr :winheight (nvim.win.get_height winid)))
        (tset vim.b bufnr :winid nil))
      (autocmd :BufWinEnter {:buffer bufnr}
        "Restore window height from the remembered value"
        (let [win nvim.current.win]
          (tset vim.b bufnr :winid win.id)
          (match (. vim.b bufnr :winheight)
            height (win:set_height height)))))
    bufnr))

(fn open-term-win [cmd ?tag]
  (var bufnr nil)
  (when ?tag
    (each [_ b (ipairs (nvim.list_bufs)) :until bufnr]
      (when (. vim.b b ?tag)
        (set bufnr b))))
  (when (not bufnr)
    (set bufnr (create-term-buf cmd ?tag)))
  (match (. vim.b bufnr :winid)
    (where winid (nvim.win.is_valid winid)) (set nvim.current.win winid)
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

(vim.cmd "cnoreabbrev <expr> term     (getcmdtype() ==# ':' && getcmdline() ==# 'term') ? 'Term'  : 'term'")
