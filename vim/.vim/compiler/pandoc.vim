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

let s:pandoc_data_dir = $HOME . '/.local/share/pandoc'

CompilerSet makeprg=pandoc\ -s\ --katex\ --metadata\ pagetitle='%:t:r'

if filereadable($HOME . '/.local/share/pandoc/filters/links-to-html.lua')
  " Add links-to-html.lua filter
  CompilerSet makeprg+=\ --lua-filter=\"$HOME/.local/share/pandoc/filters/links-to-html.lua\"
endif

if expand('%:t') ==? 'README.md' && filereadable($HOME . '/.local/share/pandoc/templates/github.html')
  " Use GFM for README files
  CompilerSet makeprg+=\ --template=\"$HOME/.local/share/pandoc/templates/github.html\"
elseif filereadable($HOME . '/.local/share/pandoc/templates/default.html')
  " Use custom pandoc template for everything else
  CompilerSet makeprg+=\ --template=\"$HOME/.local/share/pandoc/templates/default.html\"
endif

CompilerSet makeprg+=\ -o\ %:r.html\ %:S
CompilerSet errorformat=

let &cpo = s:save_cpo
unlet s:save_cpo
