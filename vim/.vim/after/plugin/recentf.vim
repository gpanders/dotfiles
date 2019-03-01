" recentf.vim configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-02-28

if !get(g:, 'loaded_recentf', 0)
  finish
endif

let g:recentf_blacklist = [
      \ 'vim/.*doc/.\+\.txt$',
      \ '^fugitive://',
      \ '/\.git/.\+'
      \ ]

nnoremap <silent> <Bslash>r :Recentf<CR>
