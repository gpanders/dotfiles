if !get(g:, 'loaded_fugitive')
  finish
endif

augroup fugitive#
  autocmd!
  autocmd BufRead fugitive://* setlocal bufhidden=delete
  autocmd BufRead * if $PWD ==# $HOME | call FugitiveDetect($HOME .. '/.dotfiles') | endif
  autocmd BufRead,BufNewFile * if !empty(FugitiveGitDir()) | let &l:tags = FugitiveGitDir() .. '/tags,' .. &tags | endif
augroup END

nnoremap g<Space> :Git<Space>
nnoremap g<CR> <Cmd>Git<CR>
