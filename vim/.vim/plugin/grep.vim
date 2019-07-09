command! -nargs=+ -complete=file_in_path Grep call grep#grep(0, <q-args>)
command! -nargs=+ -complete=file_in_path LGrep call grep#grep(1, <q-args>)

nnoremap g<CR> :Grep<space>
