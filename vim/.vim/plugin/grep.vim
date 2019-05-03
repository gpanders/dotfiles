command! -nargs=+ -complete=file_in_path -bar Grep call grep#grep(<q-args>)
command! -nargs=+ -complete=file_in_path -bar LGrep call grep#lgrep(<q-args>)

nnoremap <Bslash><Bslash> :Grep<space>
