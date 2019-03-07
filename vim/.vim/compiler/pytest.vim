" Vim compiler file
" Compiler: Py.test <http://pytest.org/>
" Maintainer: Whyme Lyu <5longluna at gmail.com>
" URL: https://github.com/5long/pytest-vim-compiler

if exists("current_compiler")
  finish
endif
let current_compiler = "pytest"

if exists(":CompilerSet") != 2 " older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

let s:cpo_save = &cpo
set cpo-=C

CompilerSet makeprg=py.test\ --tb=short\ -q

CompilerSet errorformat=
  \%*[_]\ %m\ %*[_],
  \%A%>%f:%l:\ %.%#,
  \%-Z>%*[\ ]%m,
  \E%m,
  \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save
