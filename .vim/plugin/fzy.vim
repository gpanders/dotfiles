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

nnoremap <silent> <C-P> :<C-U>call fzy#files()<CR>
nnoremap <silent> <Space>t :<C-U>call fzy#tags()<CR>

let &cpo = s:save_cpo
unlet s:save_cpo
