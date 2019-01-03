" Clang-tidy compiler plugin
" Maintainer: Greg Anders <greg@gpanders.com>
" Last Updated: 1/2/2019

if exists('current_compiler')
  finish
endif
let current_compiler = 'clangtidy'

CompilerSet makeprg=clang-tidy\ -quiet\ %:S
CompilerSet errorformat=
      \%-G%*\\d\ %s\ generated.,
      \%-GError\ while\ processing\ %f.,
      \%E%f:%l:%c:\ fatal\ error:\ %m,
      \%E%f:%l:%c:\ error:\ %m\ [%s],
      \%W%f:%l:%c:\ warning:\ %m\ [%s],
      \%I%f:%l:%c:\ note:\ %m,
      \%-Z%p%^,
      \%C%.%#
      " \%-G%\m%\%%(LLVM\ ERROR:%\|No\ compilation\ database\ found%\)%\@!%.%#,
