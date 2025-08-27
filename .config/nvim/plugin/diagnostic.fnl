(vim.diagnostic.config {:virtual_text {:severity {:min vim.diagnostic.severity.ERROR}}
                        :underline true
                        :float {:header ""}
                        :signs {:text {vim.diagnostic.severity.ERROR "✗"
                                       vim.diagnostic.severity.WARN "▲"
                                       vim.diagnostic.severity.INFO "∙"
                                       vim.diagnostic.severity.HINT "∴"}
                                :severity {:min vim.diagnostic.severity.INFO}}
                        :jump {:on_jump #(vim.diagnostic.open_float) :wrap false}
                        :severity_sort true})

(keymap :n "yog" #(vim.diagnostic.enable (not (vim.diagnostic.is_enabled)))
                 {:desc "Toggle diagnostics"})
