if !get(g:, 'loaded_dirvish')
    finish
endif

command! -bar -nargs=? -complete=dir Explore call dirvish#open(<q-args>)
command! -nargs=? -complete=dir Sexplore belowright split | silent Dirvish <args>
command! -nargs=? -complete=dir Hexplore belowright split | silent Dirvish <args>
command! -nargs=? -complete=dir Vexplore leftabove vsplit | silent Dirvish <args>

augroup plugin_dirvish
    autocmd!
    autocmd FileType dirvish nmap <silent> <buffer> q <Plug>(dirvish_quit)
augroup END
