(local uv vim.loop)

(fn generate-tags [args]
  (let [stderr (uv.new_pipe false)
        chunks []]
    (fn on-exit [code]
      (when (> (length chunks) 0)
        (let [msg (: "ctags finished with exit code %d\n%s" :format code (table.concat chunks))]
          (vim.notify msg vim.log.levels.ERROR))))
    (local args ["ctags" "-R" args])
    (when (uv.fs_stat ".git")
      (table.insert args "-o .git/tags"))
    (uv.spawn
      vim.o.shell
      {:args ["-c" (table.concat args " ")] :stdio [nil nil stderr]}
      (vim.schedule_wrap on-exit))
    (stderr:read_start
      (fn [err data]
        (assert (not err) err)
        (when data
          (table.insert chunks data))))))

(command :Tags {:nargs :*} #(generate-tags $3))
