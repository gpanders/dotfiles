" pandoc compiler file
" Compiler:         Pandoc
" Maintainer:       Greg Anders <greg@gpanders.com>
" Latest Revision:  2019-11-15

if exists('current_compiler')
  finish
endif
let current_compiler = 'pandoc'

let s:pandoc_data_dir = $HOME . '/.local/share/pandoc'

CompilerSet makeprg=pandoc\ -s\ --katex\ --metadata\ pagetitle='%:t:r'

if &filetype ==# 'markdown' && expand('%:t:r') ==? 'README' && filereadable(s:pandoc_data_dir . '/templates/github.html')
  " Use GFM for README files
  exe 'CompilerSet makeprg+=\ --from\ gfm\ --template=\"' . s:pandoc_data_dir . '/templates/github.html\"'
elseif filereadable(s:pandoc_data_dir . '/templates/default.html')
  " Use custom pandoc template for everything else
  exe 'CompilerSet makeprg+=\ --template=\"' . s:pandoc_data_dir . '/templates/default.html\"'
endif

CompilerSet makeprg+=\ -o\ %:r.html\ %:S
CompilerSet errorformat=
