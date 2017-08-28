" Enable deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_delay = 0

autocmd InsertLeave,CompleteDone * silent! pclose!

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1] =~ '\s'
endfunction

inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" : 
            \ <SID>check_back_space() ? "\<TAB>" :
            \ deoplete#mappings#manual_complete() 

inoremap <expr><C-h> deoplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> deoplete#smart_close_popup()."\<C-h>"

"" Sources

"" clang
if has("mac")
    let g:deoplete#sources#clang#libclang_path = '/usr/local/opt/llvm/lib/libclang.dylib'
    let g:deoplete#sources#clang#clang_header = '/usr/local/opt/llvm/include/clang'
else
    let g:deoplete#sources#clang#libclang_path = '/usr/local/lib/libclang.so'
    let g:deoplete#sources#clang#clang_header = '/usr/local/include/clang'
endif
