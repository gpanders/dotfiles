" The fzf plugin file should really be treated as an autoloaded file, so use
" a little hack to load it on demand
if exists('g:did_fzf_hack')
    finish
endif
let g:did_fzf_hack = 1

" Block the fzf plugin from loading
let g:loaded_fzf = 1

" Add Homebrew directory to runtimepath
set runtimepath+=/usr/local/opt/fzf

" Add Debian installation directory to runtimepath
set runtimepath+=/usr/share/doc/fzf/examples

" Add local installation to runtimepath
set runtimepath+=$HOME/.fzf
