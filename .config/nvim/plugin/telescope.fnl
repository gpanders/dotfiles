(tset vim.g :loaded_telescope 1)

(command! :Telescope {:nargs "*"} (fn [_ _ args]
  (exec "delcommand Telescope")
  (tset vim.g :loaded_telescope nil)
  (exec "packadd telescope.nvim")
  (if (not vim.g.loaded_telescope)
      (vim.notify "telescope.nvim is not installed" vim.log.levels.ERROR)
      (let [telescope (require "telescope")
            themes (require "telescope.themes")
            opts (themes.get_dropdown {:previewer false :layout_config {:width 100}})]
        (telescope.setup {
          :defaults {
            :mappings {
              :i {
                "<C-u>" false
                "<Esc>" (. (require "telescope.actions") :close)
              }
            }
          }
          :pickers {
            :find_files opts
            :buffers opts
            :oldfiles opts
            :tags opts
            :lsp_dynamic_workspace_symbols opts
            :git_files opts
          }
        })
        (exec (.. "Telescope " args))))))
