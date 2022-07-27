(when vim.g.loaded_sneak_plugin
  (keymap [:n :v :o] :f "<Plug>Sneak_f")
  (keymap [:n :v :o] :F "<Plug>Sneak_F")
  (keymap [:n :v :o] :t "<Plug>Sneak_t")
  (keymap [:n :v :o] :T "<Plug>Sneak_T")

  ; vim-sneak locks options in its plugin file, before we even know if it's
  ; loaded or not. This is a hacky way to set the options after the plugin is
  ; loaded
  (let [opt vim.g.sneak#opt]
    (set opt.label 1)
    (set opt.f_reset 0)
    (set opt.t_reset 0)
    (nvim.command "unlockvar g:sneak#opt")
    (set vim.g.sneak#opt opt)
    (nvim.command "lockvar g:sneak#opt")))
