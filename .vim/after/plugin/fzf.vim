" Check for local installation if not already loaded
if !get(g:, 'loaded_fzf') && isdirectory($HOME . '/.fzf')
  silent! source ~/.fzf/plugin/fzf.vim
endif

" If fzf is still not found, give up
if !get(g:, 'loaded_fzf')
  finish
endif

nnoremap <silent> <C-P> :FZF --layout=default --tiebreak=end,length<CR>
nmap <silent> <Space>ff <C-P>

" Create Helptags command and map it to Ctrl-H
command! -bang Helptags call fzf#helptags(<bang>0)
nnoremap <silent> <C-H> :<C-U>Helptags<CR>

command! -bang -nargs=? Tag call fzf#tags(<bang>0, <q-args>, '')
command! -bang -nargs=? Ptag call fzf#tags(<bang>0, <q-args>, 'p')
nnoremap <Space>ft :<C-U>Tag<CR>
nnoremap <C-W><Space>ft :<C-U>call fzf#tags(0, '', 's')<CR>

nnoremap <Space>fb :<C-U>call fzf#buffers(0, '')<CR>
nnoremap <C-W><Space>fb :<C-U>call fzf#buffers(0, 's')<CR>

" Hide statusline in FZF buffers
augroup plugin.fzf
    autocmd!
    autocmd FileType fzf set laststatus=0 noruler | autocmd BufLeave <buffer> set laststatus=2 ruler
augroup END
