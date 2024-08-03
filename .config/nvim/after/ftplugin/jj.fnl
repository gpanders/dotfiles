(let [buf (nvim.get_current_buf)
      fname (nvim.buf_get_name buf)]
  (autocmd ft/jj :VimLeavePre "*"
    #(let [content (with-open [f (io.open fname :r)]
                     (f:read "*a"))
           lines (icollect [line (vim.gsplit content "\n") &until done?]
                   (if (line:match "^JJ:%s*%-+ >8 %-+$")
                       (set done? true)
                       line))]
       (with-open [f (io.open fname :w)]
         (f:write (table.concat lines "\n"))))))
