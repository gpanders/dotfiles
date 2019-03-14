" ALE configuration
" This file is executed BEFORE ALE is loaded
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-02-27

if !has('nvim') && v:version < 801
  finish
endif

silent! packadd ale

" This variable must be set before ALE is loaded so that it can create the
" proper autocmds
let g:ale_lint_on_text_changed = 'normal'
