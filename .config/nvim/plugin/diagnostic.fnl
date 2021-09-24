(when vim.diagnostic
  (vim.diagnostic.config {:virtual_text false :underline false :severity_sort true})
  (local ns (vim.api.nvim_create_namespace :diagnostics))
  (fn print-diagnostics []
    (let [bufnr (vim.api.nvim_get_current_buf)
          [lnum _] (vim.api.nvim_win_get_cursor 0)
          lnum (- lnum 1)
          diagnostics (vim.diagnostic.get bufnr {: lnum})]
      (vim.diagnostic.show ns bufnr diagnostics {:signs false :virtual_text {:source :if_many}})))
  (autocmd diagnostics :BufWinEnter "*"
    (vim.diagnostic.disable)
    (autocmd diagnostics :BufWritePost "<buffer=abuf>"
      (vim.diagnostic.enable)
      (autocmd diagnostics [:CursorMoved :CursorHold :InsertLeave] "<buffer=abuf>" (print-diagnostics))))
  (autocmd diagnostics :User :DiagnosticsChanged (vim.diagnostic.setloclist {:open false}))

  (keymap :n "]g" "<Cmd>lua vim.diagnostic.goto_next { enable_popup = false }<CR>")
  (keymap :n "[g" "<Cmd>lua vim.diagnostic.goto_prev { enable_popup = false }<CR>"))
