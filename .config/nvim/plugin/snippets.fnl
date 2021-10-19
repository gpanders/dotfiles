(macro t [key]
  `(vim.api.nvim_replace_termcodes ,key true true true))

(autocmd snippets :InsertEnter "*" :once
  (with-module [snippets :snippets]
    (local U (require "snippets.utils"))

    (set snippets.ux (require "snippets.inserters.extmarks"))

    (global split_getopts (fn [str]
                            (-> (icollect [c (str:gmatch "(%a[:]?)")]
                                  (if (c:find ":")
                                      (: "\t\t%s) echo \"$OPTARG\" ;;" :format (c:sub 1 1))
                                      (: "\t\t%s) ;;" :format c)))
                                (table.concat "\n"))))

    (local my-snippets {})
    (local ft->ext {:_global :txt
                    :python :py
                    :rust :rs})
    (local include-map {:cpp [:c]})
    (local snippets-dir (.. (vim.fn.stdpath "config") "/snippets"))

    (fn read-snippets [ft]
      (local s {})
      (let [ext (or (. ft->ext ft) ft)
            handle (vim.loop.fs_scandir snippets-dir)]
        (each [fname type #(vim.loop.fs_scandir_next handle)]
          (match (fname:match "^([^/]+)%.([^.]+)$")
            (name ext) (when (= type :file)
                         (var snippet (with-open [f (io.open (.. snippets-dir "/" fname))]
                                        (: (f:read "*a") :gsub "\n$" "")))
                         (when (= ext :txt)
                           (set snippet (U.force_comment snippet)))
                         (set snippet (U.match_indentation snippet))
                         (tset s name snippet)))))
      (each [_ v (ipairs (or (. include-map ft) []))]
        (each [name snippet (pairs (read-snippets v))]
          (when (not (. s name))
            (tset s name snippet))))
      s)

    (fn check-snippet []
      (or (snippets.has_active_snippet)
          (let [(_ snippet) (snippets.lookup_snippet_at_cursor)]
            (not= snippet nil))))

    (fn setup-buffer []
      (let [ft vim.bo.filetype]
        (when (not (. my-snippets ft))
          (tset my-snippets ft (read-snippets ft))
          (set snippets.snippets my-snippets))
        (keymap :i "<Tab>"
                (fn []
                  (if (check-snippet)
                      (t "<Cmd>lua require('snippets').expand_or_advance(1)<CR>")
                      (t "<Tab>")))
                {:expr true :buffer true})
        (keymap :i "<S-Tab>" "<Cmd>lua require('snippets').advance_snippet(-1)<CR>" {:buffer true})
        (autocmd snippets :InsertLeave "<buffer>"
          (snippets.cancel))))

    (tset my-snippets :_global (read-snippets :_global))
    (set snippets.snippets my-snippets)

    (setup-buffer)
    (autocmd snippets :FileType "*" (setup-buffer))))
