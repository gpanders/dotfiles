(local {: commands} (require :treesitter))

(macro github [path]
  `,(: "https://github.com/%s" :format path))

(local parser-src {:python (github "tree-sitter/tree-sitter-python")
                   :lua (github "MunifTanjim/tree-sitter-lua")
                   :rust (github "tree-sitter/tree-sitter-rust")
                   :fennel (github "travonted/tree-sitter-fennel")
                   :zig (github "maxxnino/tree-sitter-zig")})

(local temp-dir (.. (vim.fn.stdpath :cache) "/treesitter"))
(local parser-dir (.. (vim.fn.stdpath :data) "/site/parser/"))

(fn run-in-dir [dir cmd]
  "Run cmd in the directory given by dir"
  (let [curdir (vim.fn.chdir dir)
        out (vim.fn.system cmd)
        _ (vim.fn.chdir curdir)]
    out))

(fn generate [src-dir]
  (let [abi-version vim.treesitter.language_version
        out (run-in-dir src-dir ["tree-sitter" "generate" "--abi" abi-version (.. src-dir "/grammar.js")])]
    (assert (= 0 vim.v.shell_error) out)
    (icollect [_ v (ipairs (vim.fn.globpath (.. src-dir "/src") "*" false true))]
      (let [ext (vim.fn.fnamemodify v ":e")]
        (when (or (= ext :c) (= ext :cc) (= ext :cpp))
          v)))))

(fn infer-compiler [srcs]
  (var compiler (or vim.env.CC "cc"))
  (var done? false)
  (each [_ v (ipairs srcs) :until done?]
    (let [ext (vim.fn.fnamemodify v ":e")]
      (when (or (= ext :cc) (= ext :cpp))
        (set compiler (or vim.env.CXX "c++"))
        (set done? true))))
  compiler)

(fn compile [lang src-dir build-dir srcs]
  (let [parser (: "%s/%s.so" :format build-dir lang)
        args [(infer-compiler srcs)
              "-Os"
              "-shared"
              "-I" (.. src-dir "/src")]]
    (each [_ source (ipairs srcs)]
      (table.insert args source))
    (table.insert args "-o")
    (table.insert args parser)
    (let [out (run-in-dir src-dir args)]
      (assert (= 0 vim.v.shell_error) out))))

(fn clone [lang]
  (match (. parser-src lang)
    repo (let [dir (.. temp-dir "/" lang)]
           (match (vim.fn.isdirectory dir)
             1 (let [out (vim.fn.system ["git" "-C" dir "pull"])]
                 (assert (= 0 vim.v.shell_error) out))
             _ (let [out (vim.fn.system ["git" "clone" "--depth" "1" "--quiet" repo dir])]
                 (assert (= 0 vim.v.shell_error) out)))
           dir)))

(fn commands.install [lang]
  (match (clone lang)
    src-dir (let [srcs (generate src-dir)]
              (compile lang src-dir parser-dir srcs)
              (nvim.echo [[(: "Successfully installed parser for %s" :format lang)]] false {}))
    nil (nvim.err_writeln (: "No source is known for language %s" :format lang))))
