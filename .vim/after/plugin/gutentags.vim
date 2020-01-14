if !get(g:, 'loaded_gutentags')
    finish
endif

let s:save_cpo = &cpo
set cpo&vim

let g:gutentags_ctags_extra_args = ['--tag-relative=yes']
let g:gutentags_ctags_exclude = ['build']
let g:gutentags_file_list_command = {
            \ 'markers': {
            \   '.git': 'git ls-files -co --exclude-standard',
            \ }}

" Keep tags files in git repositories under .git/tags
function! GutentagsInitUserFunc(path) abort
    try
        let root = gutentags#get_project_root(a:path)
        if !empty(root) && isdirectory(root . '/.git')
            let b:gutentags_tagfile = '.git/tags'
        endif
    catch /^gutentags\:/
    endtry
    return 1
endfunction

let g:gutentags_init_user_func = 'GutentagsInitUserFunc'

let &cpo = s:save_cpo
unlet s:save_cpo
