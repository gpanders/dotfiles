(vim.diagnostic.config {:virtual_text {:severity {:min vim.diagnostic.severity.ERROR}}
                        :underline true
                        :float {:border :rounded}
                        :signs {:severity {:min vim.diagnostic.severity.INFO}}
                        :jump {:float true :wrap false}
                        :severity_sort true})

(keymap :n "yog" #(vim.diagnostic.enable (not (vim.diagnostic.is_enabled)))
                 {:desc "Toggle diagnostics"})
