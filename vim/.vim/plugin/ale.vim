" ALE configuration
" This file is executed BEFORE ALE is loaded
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-02-27

if !has('nvim') && v:version < 801
  finish
endif

" This variable must be set before ALE is loaded so that it can create the
" proper autocmds
let g:ale_lint_on_text_changed = 'normal'

let g:ale_completion_enabled = 1

if get(g:, 'ale_completion_enabled')
  " See :h ale-completion-completeopt-bug
  set completeopt=menu,menuone,preview,noselect,noinsert
endif

silent! packadd ale
