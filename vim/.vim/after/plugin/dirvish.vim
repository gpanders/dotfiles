" vim-dirvish configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-06

if !get(g:, 'loaded_dirvish')
  unlet g:loaded_netrwPlugin
  runtime plugin/netrwPlugin.vim
  finish
endif

command! -bar -nargs=? -complete=dir Explore call dirvish#open(<q-args>)
command! -nargs=? -complete=dir Sexplore belowright split | silent Dirvish <args>
command! -nargs=? -complete=dir Hexplore belowright split | silent Dirvish <args>
command! -nargs=? -complete=dir Vexplore leftabove vsplit | silent Dirvish <args>
