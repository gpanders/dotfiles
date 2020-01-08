" GHDL compiler plugin
" Maintainer: Greg Anders <greg@gpanders.com>
" Last Updated: 1/8/2019

if exists('current_compiler')
  finish
endif
let current_compiler = 'ghdl'

CompilerSet makeprg=ghdl\ -s\ --std=08\ %:S
CompilerSet errorformat=%f:%l:%c:%trror:\ %m,%f:%l:%c:%tarning:\ %m
