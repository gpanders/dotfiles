(fn format-expr []
  (let [start (- vim.v.lnum 1)
        end (+ start vim.v.count)
        lines (vim.api.nvim_buf_get_lines 0 start end true)]
    (var tabstop 8)
    (each [i line (ipairs lines)]
      (each [elem (line:gmatch "([^,]+)")]
        (let [len (length elem)]
          (when (> len tabstop)
            (set tabstop len))))
      (if (line:find "\t")
          (tset lines i (line:gsub "\t" ","))
          (tset lines i (line:gsub "," "\t"))))
    (setlocal tabstop (+ tabstop 1))
    (vim.api.nvim_buf_set_lines 0 start end true lines)))

{ : format-expr }
