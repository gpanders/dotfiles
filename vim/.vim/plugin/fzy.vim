" fzy
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-10-02

if exists('g:loaded_fzy') || !executable('fzy')
    finish
endif
let g:loaded_fzy = 1

if executable('fd')
    let g:fzy_find_files_cmd = 'fd --type f --hidden --exclude .git'
elseif executable('rg')
    let g:fzy_find_files_cmd = 'rg --files --hidden --glob !.git'
elseif executable('ag')
    let g:fzy_find_files_cmd = 'ag -g '''''
elseif executable('find')
    let g:fzy_find_files_cmd = 'find . -type f'
endif

augroup plugin.fzy
    autocmd!
    " If in a git repo and using vim-fugitive, use git ls-files
    autocmd User Fugitive let b:fzy_find_files_cmd = 'git ls-files -co --exclude-standard'
augroup END

nnoremap <silent> <C-P> :call fzy#files()<CR>
nnoremap <silent> <Space>] :call fzy#tags()<CR>
