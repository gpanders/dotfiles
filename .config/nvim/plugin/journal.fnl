(local journal-file (case vim.env.JOURNAL_FILE
                      (where f (not= f "")) f
                      _ (vim.fs.joinpath (vim.fn.stdpath :data) "journal.md")))

(fn make-entry []
  (let [date (vim.fn.strftime "%a, %d %b, %Y")]
    (when (= 0 (vim.fn.search date ""))
      (let [lines [date (string.rep "=" (length date)) ""]]
        (vim.api.nvim_buf_set_lines 0 0 0 1 lines)
        (vim.api.nvim_win_set_cursor 0 [(+ (length lines) 1) 1])))))

(augroup journal#
  (autocmd [:BufNewFile :BufRead] journal-file #(make-entry))
  (autocmd :BufWinEnter journal-file
    #(do
       (set vim.wo.foldlevel 0)
       (let [view (vim.fn.winsaveview)]
        (exec "silent! normal! 1Gzo")
        (vim.fn.winrestview view))
       nil)))

(command :Journal {} (: "tabnew %s" :format journal-file))
(keymap :n "zJ" "<Cmd>Journal<CR>")
