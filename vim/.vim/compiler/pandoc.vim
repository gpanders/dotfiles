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

if expand('%:t') ==? 'README.md' && filereadable(s:pandoc_data_dir . '/templates/github.html')
  " Use GFM for README files
  exe 'CompilerSet makeprg+=\ --from\ gfm\ --template=\"' . s:pandoc_data_dir . '/templates/github.html\"'
elseif filereadable(s:pandoc_data_dir . '/templates/default.html')
  " Use custom pandoc template for everything else
  exe 'CompilerSet makeprg+=\ --template=\"' . s:pandoc_data_dir . '/templates/default.html\"'
endif

CompilerSet makeprg+=\ -o\ %:r.html\ %:S
CompilerSet errorformat=

let &cpo = s:save_cpo
unlet s:save_cpo
