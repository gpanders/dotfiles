command! -nargs=+ -complete=file_in_path Grep  call grep#grep(0, <q-args>)
command! -nargs=+ -complete=file_in_path LGrep call grep#grep(1, <q-args>)

cnoreabbrev <expr> gr    (getcmdtype() ==# ':' && getcmdline() ==# 'gr')    ? 'Grep'  : 'grep'
cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() ==# 'grep')  ? 'Grep'  : 'grep'
cnoreabbrev <expr> lgr   (getcmdtype() ==# ':' && getcmdline() ==# 'lgr')   ? 'LGrep' : 'lgrep'
cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() ==# 'lgrep') ? 'LGrep' : 'lgrep'

nnoremap g<Space> :Grep<Space>
