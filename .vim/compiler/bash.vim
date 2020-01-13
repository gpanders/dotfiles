if exists('current_compiler')
  finish
endif
let current_compiler = 'bash'

CompilerSet makeprg=bash\ -n\ --\ %:S
CompilerSet errorformat=%f:\ line\ %l:\ %m
