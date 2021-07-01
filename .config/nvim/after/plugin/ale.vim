if !get(g:, 'loaded_ale')
    finish
endif

autocmd User LspAttached ++once lua require('config.ale')
