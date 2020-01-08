if exists('current_compiler')
  finish
endif
let current_compiler = 'shellcheck'

CompilerSet makeprg=shellcheck\ -f\ gcc\ --\ %:S
CompilerSet errorformat=%f:%l:%c:\ %m\ [SC%n]
