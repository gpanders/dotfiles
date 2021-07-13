command! -nargs=+ -complete=file_in_path Grep  lua require('grep')(false, <q-args>, <q-mods>)
command! -nargs=+ -complete=file_in_path LGrep lua require('grep')(true, <q-args>, <q-mods>)

cnoreabbrev <expr> gr    (getcmdtype() ==# ':' && getcmdline() ==# 'gr')    ? 'Grep'  : 'gr'
cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() ==# 'grep')  ? 'Grep'  : 'grep'
cnoreabbrev <expr> lgr   (getcmdtype() ==# ':' && getcmdline() ==# 'lgr')   ? 'LGrep' : 'lgr'
cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() ==# 'lgrep') ? 'LGrep' : 'lgrep'
