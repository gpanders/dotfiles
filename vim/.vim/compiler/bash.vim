" bash compiler for Vim
" Compiler: bash
" Maintainer: Greg Anders <greg@gpanders.com>
" Last Change: 2019-11-15

if exists('current_compiler')
  finish
endif
let current_compiler = 'bash'

CompilerSet makeprg=bash\ -n\ --\ %:S
CompilerSet errorformat=%f:\ line\ %l:\ %m
