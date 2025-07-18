(when (= 1 (vim.fn.executable "prettier"))
  (let [buf (vim.api.nvim_get_current_buf)
        fname (vim.api.nvim_buf_get_name buf)]
    (vim.system
      ["prettier" "--find-config-path" fname]
      {}
      (fn [{: code : stdout}]
        (let [config (vim.trim stdout)]
          (when (and (= 0 code) (= 1 (vim.fn.filereadable config)))
            (vim.schedule #(do
                             (tset vim.bo buf :formatprg (: "prettier --config %s --stdin-filepath %s" :format config fname))
                             (tset vim.bo buf :formatexpr "")))))))))
