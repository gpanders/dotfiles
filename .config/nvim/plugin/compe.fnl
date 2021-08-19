(autocmd :my-compe :InsertEnter "*" :once
  (exec "packadd nvim-compe")
  (when vim.g.loaded_compe
    (exec "runtime! after/plugin/compe.vim")
    (local compe (require :compe))
    (compe.setup {
      :source {
        :path true
        :nvim_lsp true
        :nvim_lua true
      }
    })

    (set vim.opt.completeopt [:menuone :noselect])
    (vim.opt.shortmess:append :c)

    (keymap :i "<CR>" "compe#confirm('<CR>')" {:expr true})
    (keymap :i "<C-E>" "compe#close('<C-E>')" {:expr true})

    (exec "doautocmd <nomodeline> InsertEnter")))
