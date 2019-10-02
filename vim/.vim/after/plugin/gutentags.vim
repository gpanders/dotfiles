" vim-gutentags configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-03

if !get(g:, 'loaded_gutentags')
    finish
endif

let g:gutentags_generate_on_new = 1
let g:gutentags_generate_on_missing = 1
let g:gutentags_generate_on_write = 1
let g:gutentags_generate_on_empty_buffer = 0
let g:gutentags_ctags_extra_args = ['--tag-relative=yes']
let g:gutentags_ctags_exclude = ['build']

if executable('fd')
    let g:gutentags_file_list_command = 'fd --type f'
endif

" Keep tags files in git repositories under .git/tags
function! s:setup()
    try
        let root = gutentags#get_project_root(expand('%:p:h', 1))
        if !empty(root) && isdirectory(root . '/.git')
            let b:gutentags_tagfile = '.git/tags'
        endif
    catch /^gutentags\:/
    endtry
endfunction

augroup plugin.gutentags
    autocmd!
    autocmd BufReadPre * call <SID>setup()
augroup END
