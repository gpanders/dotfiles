if exists('current_compiler')
  finish
endif
let current_compiler = 'ghdl'

CompilerSet makeprg=ghdl\ -s\ --std=08\ %:S
CompilerSet errorformat=%f:%l:%c:%trror:\ %m,%f:%l:%c:%tarning:\ %m
