if g:os ==# 'Windows'
  let g:vimwiki_path = $HOME . '\vimfiles\wiki\'
else
  let g:vimwiki_path = $HOME . '/.vim/wiki/'
endif

silent! execute 'source ' . g:vimwiki_path . 'config.vim'
