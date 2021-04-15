if !get(g:, 'loaded_fzf')
  finish
endif

nnoremap <silent> <C-P> :<C-U>call fzf#files()<CR>
nnoremap <silent> <Space>b :<C-U>call fzf#buffers(0, '')<CR>
nnoremap <silent> <C-W><Space>b :<C-U>call fzf#buffers(0, 's')<CR>

" Hide statusline in FZF buffers
augroup plugin_fzf
    autocmd!
    autocmd FileType fzf set laststatus=0 noruler | autocmd BufLeave <buffer> set laststatus=2 ruler

    " <Esc> is mapped to <C-\><C-N> in terminal mode in vimrc, which screws up
    " the FZF terminal buffers. For FZF buffers only, make <Esc> send <Esc>
    autocmd FileType fzf tnoremap <Esc> <Esc>
augroup END
