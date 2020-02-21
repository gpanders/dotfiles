" fzy
" Author: Greg Anders <greg@gpanders.com>

if exists('g:loaded_fzy') || !executable('fzy')
    finish
endif
let g:loaded_fzy = 1

let s:save_cpo = &cpo
set cpo&vim

if executable('fd')
    let g:fzy_find_files_cmd = 'fd --type f'
elseif executable('rg')
    let g:fzy_find_files_cmd = 'rg --files'
elseif executable('ag')
    let g:fzy_find_files_cmd = 'ag -g '''''
elseif executable('find')
    let g:fzy_find_files_cmd = 'find . -type f'
endif

augroup plugin.fzy
    autocmd!
    " If in a git repo use git ls-files
    autocmd BufReadPost *
                \ if get(systemlist('git rev-parse --is-inside-work-tree'), 0, '') ==# 'true' |
                \   let b:fzy_find_files_cmd = 'git ls-files -co --exclude-standard' |
                \ endif
augroup END

nnoremap <silent> <Space>ff :<C-U>call fzy#files()<CR>
nnoremap <silent> <C-W><Space>ff :<C-U>call fzy#files({'split': 1})<CR>
nnoremap <silent> <Space>ft :<C-U>call fzy#tags()<CR>
nnoremap <silent> <C-W><Space>ft :<C-U>call fzy#tags({'split': 1})<CR>
nnoremap <silent> <Space>fb :<C-U>call fzy#buffers()<CR>
nnoremap <silent> <C-W><Space>fb :<C-U>call fzy#buffers({'split': 1})<CR>

let &cpo = s:save_cpo
unlet s:save_cpo
