command! -nargs=+ -complete=file_in_path Grep  call grep#grep(0, <q-args>)
command! -nargs=+ -complete=file_in_path LGrep call grep#grep(1, <q-args>)

cnoreabbrev <expr> gr    (getcmdtype() ==# ':' && getcmdline() ==# 'gr')    ? 'Grep'  : 'gr'
cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() ==# 'grep')  ? 'Grep'  : 'grep'
cnoreabbrev <expr> lgr   (getcmdtype() ==# ':' && getcmdline() ==# 'lgr')   ? 'LGrep' : 'lgr'
cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() ==# 'lgrep') ? 'LGrep' : 'lgrep'

nnoremap g<CR> :Grep<Space><C-R><C-W>
