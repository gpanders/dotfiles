" vimwiki configuration
" Thihs file is executed BEFORE vimwiki is loaded
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-29

let g:vimwiki_path = $MYVIMRUNTIME . '/wiki/'

" Allow per-machine wiki configuration in g:vimwiki_path/config.vim
" Silently fail if file does not exist
silent! execute 'source ' . g:vimwiki_path . 'config.vim'
