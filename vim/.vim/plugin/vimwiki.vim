let g:vimwiki_map_prefix = '<Bslash>w'
if g:os ==# 'Windows'
  let g:vimwiki_path = $HOME . '\vimfiles\wiki\'
else
  let g:vimwiki_path = $HOME . '/.vim/wiki/'
endif
