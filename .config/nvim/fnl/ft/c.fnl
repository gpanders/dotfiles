(local cc (or (os.getenv :CC) :cc))
(local cxx (or (os.getenv :CXX) :c++))

(fn path [ft]
  (let [path (.. (. vim.g (.. ft "_path")) "," vim.o.path)]
    (-> path
        (vim.split ",")
        (vim.fn.uniq)
        (table.concat ",")
        (->> (tset vim.opt_local :path))))

  (when (vim.fn.isdirectory :include)
    (: vim.opt_local.path :prepend :include))

  (doto vim.opt_local.path
    (: :remove ".")
    (: :prepend ".")))

(fn tags [update]
  (let [ft vim.bo.filetype
        compiler (if (= ft :cpp) cxx cc)
        tagfile (.. (vim.fn.stdpath :cache) "/tags/" (string.gsub (vim.fn.expand "%:p") "/" "%%") ".tags")]
    (: vim.opt_local.tags :append tagfile)
    (-> vim.bo.tags
        (vim.split ",")
        (vim.fn.uniq)
        (table.concat ",")
        (->> (tset vim.opt_local :tags)))

    (when (or update (not (vim.loop.fs_access tagfile :R)))
      (vim.fn.mkdir (string.match tagfile "^(.+)/.-$") :p)
      (var cmd (.. compiler " -M -Iinclude " (vim.fn.expand "%") "| awk '{for (i=1; i<=NF; i++) if ($i ~ /.h$/) print $i}' | ctags -L - -o " tagfile))
      (if (= ft :c)
          (set cmd (.. cmd " --c-kinds=+px --langmap=c:+.h --languages=c"))
          (set cmd (.. cmd " --c++-kinds=+px --extras=+q --language-force=c++ --languages=c++")))

      (let [stderr (vim.loop.new_pipe false)
            chunks []]
        (fn on-exit []
          (let [output (table.concat chunks)]
            (when (> (length output) 0)
              (vim.notify output vim.log.levels.ERROR))))
        (vim.loop.spawn
          vim.o.shell
          {:args ["-c" cmd] :stdio [nil nil stderr]}
          (vim.schedule_wrap on-exit))
        (stderr:read_start
          (fn [err data]
            (assert (not err) err)
            (when data
              (table.insert chunks data))))))))

(fn set-path []
  (let [ft vim.bo.filetype]
    (if (. vim.g (.. ft "_path"))
        (path ft)
        (let [stdout (vim.loop.new_pipe false)
              stderr (vim.loop.new_pipe false)
              compiler (if (= ft :cpp) cxx cc)
              args ["-E" "-Wp,-v" "-x" (if (= ft :cpp) "c++" "c") "/dev/null"]
              chunks []]
          (fn on-exit []
            (let [lines (vim.split (vim.trim (table.concat chunks)) "\n")]
              (var paths [])
              (each [_ v (ipairs lines)]
                (when (string.match v "^ ")
                  (table.insert paths (string.match v "%S+"))))
              (-> #(vim.fn.simplify $1)
                  (vim.tbl_map paths)
                  (table.concat ",")
                  (->> (tset vim.g (.. ft "_path"))))
              (path ft)))

          (vim.loop.spawn
            compiler
            {: args :stdio [nil stdout stderr]}
            (vim.schedule_wrap on-exit))
          (let [f (fn [err data]
                    (assert (not err) err)
                    (when data
                      (table.insert chunks data)))]
            (stdout:read_start f)
            (stderr:read_start f))))))

{ : set-path : tags }
