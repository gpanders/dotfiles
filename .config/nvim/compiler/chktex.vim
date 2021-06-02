if exists('current_compiler')
  finish
endif
let current_compiler = 'chktex'

CompilerSet makeprg=chktex\ -q\ -v0\ --\ %:S
CompilerSet errorformat=%f:%l:%c:%n:%m
