{:filetypes ["rust"]
 :cmd ["rust-analyzer"]
 :root_dir (fn [buf cb]
            (let [cwd (vim.fs.dirname (vim.api.nvim_buf_get_name buf))]
              (vim.system ["cargo" "metadata" "--no-deps" "--format-version" "1"]
                          {: cwd}
                          (fn [out]
                            (when (= 0 out.code)
                              (case (pcall vim.json.decode out.stdout)
                                (true {: workspace_root}) (cb workspace_root)))))))
 :settings {:autoformat true
            :rust-analyzer {:check {:command "clippy"}}}}
