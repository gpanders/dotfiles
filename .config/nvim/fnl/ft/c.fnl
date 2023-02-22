(local cc (or (os.getenv :CC) :cc))
(local cxx (or (os.getenv :CXX) :c++))
(local cache {})

(local M {})

(fn read-compile-commands [bufnr]
  (match (io.open "compile_commands.json" :r)
    f (let [commands (vim.json.decode (f:read "*a"))
            fname (nvim.buf_get_name bufnr)
            relname (vim.fn.fnamemodify fname ":.")
            dirs []]
        (f:close)
        (var stop? false)
        (each [_ v (ipairs commands) &until stop?]
          (when (or (= fname v.file) (= relname v.file))
            (set stop? true)
            (var include-next false)
            (let [arguments (or v.arguments (vim.split v.command "%s+"))]
              (each [_ tok (ipairs arguments)]
                (if include-next
                    (do
                      (table.insert dirs tok)
                      (set include-next false))
                    (or (= tok "-I") (= tok "-isystem"))
                    (set include-next true)
                    (table.insert dirs (string.match tok "^-I(%S+)$")))))))
        (table.concat dirs ","))))

(fn callback [bufnr cc data]
  (let [paths (-> (icollect [_ line (ipairs data)]
                    (if (line:match "^ ")
                        (vim.fn.simplify (line:match "%S+"))))
                  (table.concat ","))]
    (tset cache cc paths)
    (M.set-path bufnr)))

(fn M.set-path [bufnr]
  (let [ft (. vim.bo bufnr :filetype)
        compiler (if (= ft :cpp) cxx cc)]
    (match (. cache compiler)
      nil (let [args [compiler "-E" "-Wp,-v" "-x" (if (= ft :cpp) "c++" "c") "/dev/null" "2>&1"]]
            (vim.fn.jobstart (table.concat args " ") {:stdout_buffered true
                                                      :on_stdout #(callback bufnr compiler $2)}))
      p (do
          (var path vim.o.path)
          (when (= 1 (vim.fn.isdirectory :include))
            (set path "include,"))
          (set path (.. path p))
          (match (read-compile-commands bufnr)
            v (set path (.. path "," v)))
          (tset vim.bo bufnr :path path)))))

M
