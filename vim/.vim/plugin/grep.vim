command! -nargs=+ -complete=file_in_path -bar Grep call grep#grep(0, <q-args>)
command! -nargs=+ -complete=file_in_path -bar LGrep call grep#grep(1, <q-args>)

nnoremap <Bslash><Bslash> :Grep<space>
