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
    let g:fzy_find_files_cmd = 'find -type f'
endif

nnoremap <silent> <C-P> :call fzy#files()<CR>
nnoremap <silent> <Space>] :call fzy#tags()<CR>
