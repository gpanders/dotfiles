(local nvimrcdir (.. (vim.fn.stdpath :cache) "/nvimrc"))

(fn hash-filename [path]
  (: (dirname path) :gsub "/" "%%"))

(fn get-hash [path]
  (let [fname (hash-filename path)]
    (match (pcall vim.fn.readfile (.. nvimrcdir "/" fname))
      (false _) nil
      (true [hash]) hash)))

(fn set-hash [path hash]
  (vim.fn.mkdir nvimrcdir :p)
  (let [fname (hash-filename path)]
    (vim.fn.writefile [hash] (.. nvimrcdir "/" fname))))

(fn sha256sum [file]
  (let [out (vim.fn.system ["shasum" "-a" "256" file])]
    (assert (= 0 vim.v.shell_error) out)
    ((vim.gsplit out " "))))

(fn find []
  (let [nvimrc (.. (vim.loop.cwd) "/.nvimrc")]
    (when (= 1 (vim.fn.filereadable nvimrc))
      (let [hash (sha256sum nvimrc)]
        (match (get-hash nvimrc)
          hash (exec (.. "source " nvimrc))
          _ (match (vim.fn.confirm "Found untrusted .nvimrc.\nTrust? " "&Yes\n&No\n&View" 2 :Question)
              1 (do
                  (set-hash nvimrc hash)
                  (.. "source " nvimrc))
              3 (exec (.. "sview " nvimrc))))))))

{: find}
