" Enable deoplete
let g:deoplete#enable_at_startup = 1

"" Deoplete clang
let g:deoplete#sources#clang#libclang_path = '/usr/local/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/local/include/clang'
let g:deoplete#auto_complete_delay = 0
autocmd InsertLeave,CompleteDone * silent! pclose!

" clang_complete config
let g:clang_complete_auto = 0
let g:clang_auto_select = 0
let g:clang_omnicppcomplete_compliance = 0
let g:clang_make_default_keymappings = 0

inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" : 
            \ <SID>check_back_space() ? "\<TAB>" :
            \ deoplete#mappings#manual_complete() 
            function! s:check_back_space() abort "{{{
                let col = col('.') - 1
                return !col || getline('.')[col - 1] =~ '\s'
            endfunction"}}}

inoremap <expr><C-h> deoplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> deoplete#smart_close_popup()."\<C-h>"
