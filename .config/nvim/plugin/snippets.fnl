(autocmd :my-snippets :InsertEnter "*" :once
  (local (ok snippets) (pcall require :snippets))
  (when ok
    (keymap :i "<Tab>" "v:lua.check_snippet() ? '<Cmd>lua require(\"snippets\").expand_or_advance(1)<CR>' : '<Tab>'" {:expr true})
    (keymap :i "<S-Tab>" "<Cmd>lua require('snippets').advance_snippet(-1)<CR>")

    (global check_snippet (fn []
      (if (snippets.has_active_snippet)
          true
          (let [(_ snippet) (snippets.lookup_snippet_at_cursor)]
            (not= snippet nil)))))

    (global split_getopts (fn [str]
      (-> (icollect [c (str:gmatch "(%a[:]?)")]
                    (if (c:find ":")
                        (: "\t\t%s) echo \"$OPTARG\" ;;" :format (c:sub 1 1))
                        (: "\t\t%s) ;;" :format c)))
          (table.concat "\n"))))

    (local U (require "snippets.utils"))

    (set snippets.ux (require "snippets.inserters.extmarks"))

    (local ext-ft-map {:txt :_global :py :python :rs :rust})
    (local include-map {:c [:cpp]})
    (local snippets-dir (.. (vim.fn.stdpath "config") "/snippets"))

    (fn read-snippets [t]
      (local s (or t {}))
      (each [filename (vim.gsplit (vim.fn.glob (.. snippets-dir "/*")) "\n")]
        (with-open [f (io.open filename)]
          (let [(name ext) (filename:match "^.+/([^/]+)%.([^.]+)$")
                ft (or (. ext-ft-map ext) ext)]
            (var snippet (: (f:read "*a") :gsub "\n$" ""))

            (when (= ext :txt)
              (set snippet (U.force_comment snippet)))

            (set snippet (U.match_indentation snippet))

            (when (not (. s ft))
              (tset s ft {}))
            (tset (. s ft) name snippet)

            (each [_ v (ipairs (or (. include-map ft) []))]
              (when (not (. s v))
                (tset s v {}))
              (tset (. s v) name snippet)))))
      s)

      (set snippets.snippets (read-snippets))))
