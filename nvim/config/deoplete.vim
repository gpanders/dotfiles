" Enable deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_delay = 0

autocmd InsertLeave,CompleteDone * silent! pclose!

inoremap <expr><C-h> deoplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> deoplete#smart_close_popup()."\<C-h>"

call deoplete#custom#source('ultisnips', 'rank', 1000)

"" Sources

"" clang
if has("mac")
  let g:deoplete#sources#clang#libclang_path = '/usr/local/opt/llvm/lib/libclang.dylib'
  let g:deoplete#sources#clang#clang_header  = '/usr/local/opt/llvm/include/clang'
else
  let g:deoplete#sources#clang#libclang_path = '/usr/local/lib/libclang.so'
  let g:deoplete#sources#clang#clang_header  = '/usr/local/include/clang'
endif

let g:deoplete#sources#clang#autofill_neomake = 1

"" LaTeX
function! s:load_latex_sources() abort
  if exists('g:vimtex#re#deoplete')
    if !exists('g:deoplete#omni#input_patterns')
      let g:deoplete#omni#input_patterns = {}
    endif
    let g:deoplete#omni#input_patterns.tex = g:vimtex#re#deoplete
  endif
endfunction

autocmd FileType tex call <SID>load_latex_sources()
