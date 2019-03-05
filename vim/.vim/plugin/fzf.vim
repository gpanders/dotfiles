" Homebrew does not add fzf plugin to vim runtimepath so source it manually
if get(g:, 'os', '') ==# 'Darwin' && 0
  silent! source /usr/local/opt/fzf/plugin/fzf.vim
endif

let g:loaded_fzf = 0
