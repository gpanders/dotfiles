" signify
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-05-30

if !get(g:, 'loaded_signify')
  finish
endif

" Update this list with all VCS's used
let g:signify_vcs_list = ['git', 'svn']
