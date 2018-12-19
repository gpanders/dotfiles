if !exists('g:loaded_dirvish')
  unlet g:loaded_netrwPlugin
  runtime plugin/netrwPlugin.vim
  finish
endif

command! -bar -nargs=? -complete=dir Explore call dirvish#open(<q-args>)
command! -nargs=? -complete=dir Sexplore belowright split | silent Dirvish <args>
command! -nargs=? -complete=dir Vexplore leftabove vsplit | silent Dirvish <args>
