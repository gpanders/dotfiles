" Vim compiler file
" Compiler:         Pandoc
" Maintainer:       Greg Anders <greg@gpanders.com>
" Latest Revision:  2019 Mar 13

if exists('current_compiler')
  finish
endif
let current_compiler = 'pandoc'

let s:save_cpo = &cpo
set cpo&vim

if exists(':CompilerSet') != 2
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=pandoc\ -s\ --metadata\ pagetitle=\"%:t:r\"\ -o\ %:t:r.html\ %:S
CompilerSet errorformat=

if exists('$BROWSER')
  CompilerSet makeprg+=\ &&\ $BROWSER\ %:t:r.html
endif

let &cpo = s:save_cpo
unlet s:save_cpo
