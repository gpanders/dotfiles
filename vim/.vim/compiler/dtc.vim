" DTC compiler plugin
" Maintainer: Greg Anders <greg@gpanders.com>
" Last Updated: 2019-05-08

if exists('current_compiler')
  finish
endif
let current_compiler = 'dtc'

CompilerSet makeprg=dtc\ -I\ dts\ -O\ dtb\ -o\ %<.dtb\ %
CompilerSet errorformat=%f:%l.%c-%\\d%\\+.%\\d%\\+:\ %m
