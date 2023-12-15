if !get(g:, 'loaded_fugitive')
  finish
endif

augroup fugitive#
  autocmd!
  autocmd BufRead fugitive://* setlocal bufhidden=delete
  autocmd BufRead * if $PWD ==# $HOME | call FugitiveDetect($HOME .. '/.dotfiles') | endif
  autocmd VimEnter,DirChanged,BufRead,BufNewFile * if !empty(FugitiveGitDir()) | let &l:tags = split(FugitiveGitDir() .. '/tags,' .. &tags, ',')->uniq()->join(',') | endif
  autocmd FileType fugitive,git setlocal nolist
augroup END

nnoremap g<Space> :Git<Space>
nnoremap g! :Git!<Space>
nnoremap g<CR> <Cmd>Git<CR>
