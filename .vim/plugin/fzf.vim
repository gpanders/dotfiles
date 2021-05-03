" The fzf plugin file should really be treated as an autoloaded file, so use
" a little hack to load it on demand
if exists('g:did_fzf_hack')
    finish
endif
let g:did_fzf_hack = 1

" Block the fzf plugin from loading
let g:loaded_fzf = 1
