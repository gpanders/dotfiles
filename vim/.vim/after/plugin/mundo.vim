" mundo
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-11-05

if !get(g:, 'loaded_mundo')
    finish
endif

nnoremap <Bslash>U :<C-U>MundoToggle<CR>
