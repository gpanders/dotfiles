" Check for local installation if not already loaded
if !get(g:, 'loaded_fzf') && isdirectory($HOME . '/.fzf')
  silent! source ~/.fzf/plugin/fzf.vim
endif

" If fzf is still not found, give up
if !get(g:, 'loaded_fzf')
  finish
endif

nnoremap <silent> <Space>f :FZF --layout=default --tiebreak=end,length<CR>

command! -bang -complete=tag -nargs=? Tag call fzf#tags(<bang>0, <q-args>, '')
command! -bang -complete=tag -nargs=? Stag call fzf#tags(<bang>0, <q-args>, 's')
command! -bang -complete=tag -nargs=? Ptag call fzf#tags(<bang>0, <q-args>, 'p')
nnoremap <Space>] :<C-U>Tag<Space>
nnoremap <C-W><Space>] :<C-U>Stag<Space>
nnoremap <Space>} :<C-U>Ptag<Space>

nnoremap <silent> <Space>b :<C-U>call fzf#buffers(0, '')<CR>
nnoremap <silent> <C-W><Space>b :<C-U>call fzf#buffers(0, 's')<CR>

" Hide statusline in FZF buffers
augroup plugin.fzf
    autocmd!
    autocmd FileType fzf set laststatus=0 noruler | autocmd BufLeave <buffer> set laststatus=2 ruler
augroup END
