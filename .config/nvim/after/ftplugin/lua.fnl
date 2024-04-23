(let [path (vim.fs.dirname (nvim.buf_get_name 0))
      [f] (vim.fs.find ["stylua.toml" ".stylua.toml"] {:upward true
                                                       :stop (vim.uv.os_homedir)
                                                       : path})]
  (when f
    (set vim.bo.formatprg (: "stylua -f %s -" :format f))))
(exec "setlocal comments^=:---")
