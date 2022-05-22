(fn tags-to-quickfix [tags title]
  (let [items (icollect [i v (ipairs tags)]
                (let [{: cmd : name : kind : filename} v]
                  {: filename
                   :pattern (.. "\\M" (cmd:sub 2 (- (length cmd) 1)))
                   :nr i
                   :type kind
                   :text name}))]
    (vim.fn.setqflist [] " " {: items
                              :context items
                              : title
                              :quickfixtextfunc "tags#qftf"})
    (vim.cmd "copen")))

(fn find-tags [query]
  (let [pattern (if (query:match "^/")
                    (query:sub 2)
                    (: "^%s$" :format query))]
    (vim.fn.taglist pattern)))

(fn tselect [arg]
  (let [tags (find-tags arg)]
    (tags-to-quickfix tags arg)
    tags))

(fn tjump [arg]
  (let [tags (find-tags arg)]
    (match (length tags)
      0 (match (pcall vim.cmd (: "tjump %s" :format arg))
          (false err) (nvim.err_writeln (pick-values 1 (err:gsub "^.-:" ""))))
      1 (vim.cmd (: "tjump %s" :format (. tags 1 :name)))
      n (tags-to-quickfix tags arg))))

(command :Tselect {:nargs 1 :complete :tag} (fn [{: args}] (tselect args)))
(command :Tjump {:nargs 1 :complete :tag} (fn [{: args}] (tjump args)))

(vim.cmd "
cnoreabbrev <expr> ts      (getcmdtype() ==# ':' && getcmdline() ==# 'ts')      ? 'Tselect' : 'ts'
cnoreabbrev <expr> tselect (getcmdtype() ==# ':' && getcmdline() ==# 'tselect') ? 'Tselect' : 'tselect'
cnoreabbrev <expr> tj      (getcmdtype() ==# ':' && getcmdline() ==# 'tj')      ? 'Tjump'   : 'tj'
cnoreabbrev <expr> tjump   (getcmdtype() ==# ':' && getcmdline() ==# 'tjump')   ? 'Tjump'   : 'tjump'
")
