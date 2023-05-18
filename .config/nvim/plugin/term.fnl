(local default-command :fish)

(fn create-term-buf [cmd ?tag]
  (let [bufnr (nvim.create_buf true false)]
    (nvim.buf_call bufnr #(vim.fn.termopen cmd))
    (when ?tag
      (tset vim.b bufnr ?tag true))
    (augroup term#
      (when (and (= default-command (. cmd 1)) (= 1 (length cmd)))
        (autocmd :TermClose {:buffer bufnr}
          "Automatically close window if it exits without error"
          #(when (= 0 vim.v.event.status)
             (let [exe (vim.fn.exepath (. cmd 1))]
               (match (nvim.get_chan_info vim.bo.channel)
                 {:argv [exe] :buffer bufnr} (nvim.command (.. "bdelete! " bufnr)))))))
      (autocmd :BufWinLeave {:buffer bufnr}
        "Remember height of buffer window"
        (fn []
          (match (. vim.b bufnr :winid)
            (where winid (nvim.win_is_valid winid)) (tset vim.b bufnr :winheight (nvim.win_get_height winid)))
          (tset vim.b bufnr :winid nil)))
      (autocmd :BufWinEnter {:buffer bufnr}
        "Restore window height from the remembered value"
        #(let [win (nvim.get_current_win)]
           (tset vim.b bufnr :winid win)
           (match (. vim.b bufnr :winheight)
             height (nvim.win_set_height win height)))))
    bufnr))

(fn open-term-win [cmd ?tag]
  (var bufnr nil)
  (when ?tag
    (each [_ b (ipairs (nvim.list_bufs)) &until bufnr]
      (when (. vim.b b ?tag)
        (set bufnr b))))
  (when (not bufnr)
    (set bufnr (create-term-buf cmd ?tag)))
  (match (. vim.b bufnr :winid)
    (where winid (nvim.win_is_valid winid)) (nvim.set_current_win winid)
    _ (do
        (nvim.command "botright new")
        (nvim.win_set_buf 0 bufnr)
        ; These are set by termopen(), but since they are window local options
        ; they must be set anytime a new window is created
        (set vim.wo.list false)
        (set vim.wo.wrap false)))
  (nvim.command "startinsert"))

(command :Term {:nargs "*"}
  (fn [{: args}]
    (let [args (if (= "" args) default-command args)
          cmd (vim.split args " " {})]
      (open-term-win cmd))))

(keymap :n "`<CR>" #(open-term-win default-command (: "term%d" :format vim.v.count)))
(keymap :n "`<Space>" ":<C-U>Term " {:silent false})

(vim.cmd "cnoreabbrev <expr> term     (getcmdtype() ==# ':' && getcmdline() ==# 'term') ? 'Term'  : 'term'")
