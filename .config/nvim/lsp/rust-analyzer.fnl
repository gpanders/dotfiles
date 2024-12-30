{:filetypes ["rust"]
 :cmd ["rust-analyzer"]
 :root_dir (fn [cb]
            (case (vim.fs.root 0 ["Cargo.toml"])
              root (vim.system ["cargo" "metadata" "--no-deps" "--format-version" "1"]
                               {:cwd root}
                               (fn [out]
                                 (if (not= 0 out.code)
                                     (cb root)
                                     (case (pcall vim.json.decode out.stdout)
                                       (true {: workspace_root}) (cb workspace_root)
                                       _ (cb root)))))
              _ (cb nil)))
 :settings {:autoformat true
            :rust-analyzer {:check {:command "clippy"}}}}
