(local cc (or (os.getenv :CC) :cc))
(local cxx (or (os.getenv :CXX) :c++))

(fn uniq [t]
  (var seen {})
  (icollect [_ v (ipairs t)]
    (when (not (. seen v))
      (tset seen v true)
      v)))

(fn tags [update]
  (let [ft vim.bo.filetype
        compiler (if (= ft :cpp) cxx cc)
        tagfile (: "%s/tags/%s.tags" :format (vim.fn.stdpath :cache) (: (vim.fn.expand "%:p") :gsub "/" "%%"))]
    (setlocal+= tags tagfile)
    (-> vim.bo.tags
        (vim.split ",")
        (uniq)
        (table.concat ",")
        (->> (set vim.opt_local.tags)))

    (when (or update (not (vim.loop.fs_access tagfile :R)))
      (vim.fn.mkdir (tagfile:match "^(.+)/.-$") :p)
      (local path (vim.fn.expand "%:p"))
      (when (vim.loop.fs_stat path)
        (var cmd (.. compiler " -M -Iinclude " path "| awk '{for (i=1; i<=NF; i++) if ($i ~ /.h$/) print $i}' | ctags -L - -o " tagfile))
        (if (= ft :c)
            (append! cmd " --c-kinds=+px --langmap=c:+.h --languages=c")
            (append! cmd " --c++-kinds=+px --extras=+q --language-force=c++ --languages=c++"))
        ; Prevent returning the output of uv.spawn (which is a userdata type)
        ; by binding to a throwaway variable
        (local _ (vim.loop.spawn vim.o.shell {:args ["-c" cmd]} #nil))))))

(fn set-path []
  (let [ft vim.bo.filetype
        stdout (vim.loop.new_pipe false)
        stderr (vim.loop.new_pipe false)
        compiler (if (= ft :cpp) cxx cc)
        args ["-E" "-Wp,-v" "-x" (if (= ft :cpp) "c++" "c") "/dev/null"]
        chunks []]
    (fn on-exit []
      (let [lines (vim.split (vim.trim (table.concat chunks)) "\n")
            paths (icollect [_ v (ipairs lines)]
                    (when (v:match "^ ")
                      (v:match "%S+")))]
        (-> #(vim.fn.simplify $1)
            (vim.tbl_map paths)
            (uniq)
            (table.concat ",")
            ( .. "," vim.o.path)
            (->> (set vim.opt_local.path)))
        (when (vim.fn.isdirectory :include)
          (setlocal^= path :include))

        (setlocal-= path ".")
        (setlocal^= path ".")))

    (vim.loop.spawn
      compiler
      {: args :stdio [nil stdout stderr]}
      (vim.schedule_wrap on-exit))
    (let [f (fn [err data]
              (assert (not err) err)
              (when data
                (table.insert chunks data)))]
      (stdout:read_start f)
      (stderr:read_start f))))

{
  : set-path
  : tags
}
