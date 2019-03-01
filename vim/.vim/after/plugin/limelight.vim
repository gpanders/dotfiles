" vim-limelight configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-02-19

if !exists(':Limelight')
  finish
endif

if exists(':Goyo')
  autocmd! User GoyoEnter Limelight
  autocmd! User GoyoLeave Limelight!
endif
