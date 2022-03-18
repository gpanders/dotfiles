(with-module [nvimrc :nvimrc]
  (nvimrc.find))

(local buf (setmetatable {} {:__index (fn [t k]
                                        (tset t k (. vim.api (.. "nvim_buf_" k)))
                                        (. t k))}))

(local win (setmetatable {} {:__index (fn [t k]
                                        (tset t k (. vim.api (.. "nvim_win_" k)))
                                        (. t k))}))

(local current (setmetatable {} {:__index (fn [t k]
                                            ((. vim.api (.. "nvim_get_current_" k))))
                                 :__newindex (fn [t k v]
                                               ((. vim.api (.. "nvim_set_current_" k)) v))}))

(set _G.nvim (setmetatable {: buf : win : current} {:__index (fn [t k]
                                                               (tset t k (. vim.api (.. "nvim_" k)))
                                                               (. t k))}))
