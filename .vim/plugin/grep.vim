if has('nvim') || has('job')
    command! -nargs=+ -complete=file_in_path Grep call grep#grep(0, <q-args>)
    command! -nargs=+ -complete=file_in_path LGrep call grep#grep(1, <q-args>)
else
    command! -nargs=+ -complete=file_in_path Grep silent grep <args> | redraw! | copen
    command! -nargs=+ -complete=file_in_path LGrep silent lgrep <args> | redraw! | copen
endif

nnoremap g/ :Grep<space>
nnoremap <silent> g<CR> :<C-U>Grep <C-R><C-W><CR>
