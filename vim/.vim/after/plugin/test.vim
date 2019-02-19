" Configuration for vim-test
if !exists('g:loaded_test')
  finish
endif

nnoremap <silent> t<C-N> :TestNearest<CR>
nnoremap <silent> t<C-F> :TestFile<CR>
nnoremap <silent> t<C-S> :TestSuite<CR>
nnoremap <silent> t<C-L> :TestLast<CR>
nnoremap <silent> t<C-G> :TestVisit<CR>

let test#strategy = 'dispatch'

let test#python#pytest#options = '--tb=short -q'
