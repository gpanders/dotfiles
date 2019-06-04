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

if expand('%:t') ==# 'README.md' && filereadable($HOME . '/.config/pandoc/templates/github.html')
  " Use GFM for README files
  CompilerSet makeprg=pandoc\ -s\ --katex\ --template\ \"$HOME/.config/pandoc/templates/github.html\"\ -o\ %:t:r.html\ %:S
elseif filereadable($HOME . '/.config/pandoc/templates/default.html')
  " Use custom pandoc template for everything else
  CompilerSet makeprg=pandoc\ -s\ --katex\ --template\ \"$HOME/.config/pandoc/templates/default.html\"\ -o\ %:t:r.html\ %:S
else
  " If all else fails, don't use a template at all
  CompilerSet makeprg=pandoc\ -s\ --katex\ -o\ %:t:r.html\ %:S
endif
CompilerSet errorformat=

let &cpo = s:save_cpo
unlet s:save_cpo
