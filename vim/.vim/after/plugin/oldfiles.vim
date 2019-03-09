" oldfiles.vim configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-02-28

if !get(g:, 'loaded_oldfiles', 0)
  finish
endif

let g:oldfiles_blacklist = [
      \ 'vim/.*doc/.\+\.txt$',
      \ '^fugitive://',
      \ '/\.git/.\+'
      \ ]
