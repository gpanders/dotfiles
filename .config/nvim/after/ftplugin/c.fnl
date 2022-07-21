(local {: set-path} (require "ft/c"))

(set-path (nvim.get_current_buf))

(set vim.bo.commentstring "//%s")
