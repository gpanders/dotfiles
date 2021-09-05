(macro t [key]
  `(vim.api.nvim_replace_termcodes ,key true true true))

(autocmd snippets :InsertEnter "*" :once
  (with-module [snippets :snippets]

    (fn check-snippet []
      (if (snippets.has_active_snippet)
          true
          (let [(_ snippet) (snippets.lookup_snippet_at_cursor)]
            (not= snippet nil))))

    (keymap :i "<Tab>"
            (fn []
              (if (check-snippet)
                  (t "<Cmd>lua require('snippets').expand_or_advance(1)<CR>")
                  (t "<Tab>")))
            {:expr true})
    (keymap :i "<S-Tab>" "<Cmd>lua require('snippets').advance_snippet(-1)<CR>")

    (global split_getopts (fn [str]
                            (-> (icollect [c (str:gmatch "(%a[:]?)")]
                                  (if (c:find ":")
                                      (: "\t\t%s) echo \"$OPTARG\" ;;" :format (c:sub 1 1))
                                      (: "\t\t%s) ;;" :format c)))
                                (table.concat "\n"))))

    (local U (require "snippets.utils"))

    (set snippets.ux (require "snippets.inserters.extmarks"))

    (local ext->ft {:txt :_global
                    :py :python
                    :rs :rust})
    (local include-map {:c [:cpp]})
    (local snippets-dir (.. (vim.fn.stdpath "config") "/snippets"))

    (fn read-snippets []
      (local s {})
      (each [filename (vim.gsplit (vim.fn.glob (.. snippets-dir "/*")) "\n")]
        (let [(name ext) (filename:match "^.+/([^/]+)%.([^.]+)$")
              ft (or (. ext->ft ext) ext)]
          (var snippet (with-open [f (io.open filename)]
                         (: (f:read "*a") :gsub "\n$" "")))
          (when (= ext :txt)
            (set snippet (U.force_comment snippet)))
          (set snippet (U.match_indentation snippet))
          (each [_ v (pairs (icollect [_ v (pairs (or (. include-map ft) [])) :into [ft]] v))]
            (when (not (. s v))
              (tset s v {}))
            (tset (. s v) name snippet))))
      s)

    (set snippets.snippets (read-snippets))))
