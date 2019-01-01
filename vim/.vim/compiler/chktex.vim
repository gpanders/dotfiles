" chktex compiler for Vim
" Compiler: chktex
" Maintainer: Greg Anders <greg@gpanders.com>
" Last Change: 1/1/2019

let current_compiler = 'chktex'

if exists(":CompilerSet") != 2		" older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=chktex\ -q\ -v0\ --\ %:S
CompilerSet errorformat=%f:%l:%c:%n:%m
