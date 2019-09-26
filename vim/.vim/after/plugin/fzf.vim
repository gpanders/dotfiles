" fzf configuration
" Author: Greg Anders <greg@gpanders.com>

" Check for local installation if not already loaded
if !get(g:, 'loaded_fzf') && isdirectory($HOME . '/.fzf')
  silent! source ~/.fzf/plugin/fzf.vim
endif

" If fzf is still not found, give up
if !get(g:, 'loaded_fzf')
  finish
endif

" Ctrl-P to start FZF
nnoremap <silent> <C-P> :FZF<CR>

" Create Helptags command and map it to Ctrl-H
command! -bang Helptags call fzf#helptags(<bang>0)
nnoremap <silent> <C-H> :<C-U>Helptags<CR>

command! -bang -nargs=? Tags call fzf#tags(<bang>0, '<args>', '')
nnoremap <Space>] :<C-U>Tags<CR>

command! -bang -nargs=? Ptags call fzf#tags(<bang>0, '<args>', 'p')
nnoremap <Space>} :<C-U>Ptags<CR>

" Hide statusline in FZF buffers
autocmd! FileType fzf set laststatus=0 noruler | autocmd BufLeave <buffer> set laststatus=2 ruler
