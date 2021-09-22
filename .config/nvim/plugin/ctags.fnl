(fn on-exit [_ code]
  (when (> code 0)
    (vim.notify (: "ctags failed with code %d" :format code) vim.log.levels.WARN)))

(fn generate-tags [args]
  (let [git-dir (or (os.getenv :GIT_DIR) ".git")
        output (match (?. (vim.loop.fs_stat git-dir) :type)
                 :directory (.. git-dir "/tags")
                 _ "tags")
        cmd (: "ctags -R -o %s %s" :format output args)]
    (vim.fn.jobstart cmd {:on_exit on-exit})))

(command :Tags {:nargs :*} #(generate-tags $3))
