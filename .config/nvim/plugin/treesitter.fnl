(fn complete [arg line pos]
  (let [commands (. (require :treesitter) :commands)]
    (icollect [cmd (pairs commands)]
      (if (= arg (string.sub cmd 1 (length arg)))
          cmd))))

(command :Treesitter {:nargs "+" : complete}
         (fn [{: args}]
           (let [commands (. (require :treesitter) :commands)
                 [cmd & args] (vim.split args " ")]
             (match (. commands cmd)
               f (f (unpack args))
               _ (let [matches (icollect [k (pairs commands)]
                                 (when (= cmd (string.sub k 1 (length cmd)))
                                   k))]
                   (match (length matches)
                     1 ((. commands (. matches 1)) (unpack args))
                     0 (nvim.err_writeln (: "Invalid command: %s" :format cmd))
                     _ (nvim.err_writeln (: "Ambiguous command: %s can match any of %s" :format cmd (table.concat matches ", ")))))))))
