(vim.diagnostic.config {:virtual_text false
                        :underline true
                        :float {:border :rounded}
                        :signs {:severity {:min vim.diagnostic.severity.INFO}}
                        :severity_sort true})

(keymap :n "[d" vim.diagnostic.goto_prev)
(keymap :n "]d" vim.diagnostic.goto_next)
(keymap :n "yog" #(vim.diagnostic.enable (not (vim.diagnostic.is_enabled)))
                 {:desc "Toggle diagnostics"})
