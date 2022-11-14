" Put parsers and queries on runtimepath but don't run any plugin files
packadd! nvim-treesitter

function s:playground()
    delcommand TSPlaygroundToggle
    packadd nvim-treesitter
    packadd playground
    TSPlaygroundToggle
endfunction
command! TSPlaygroundToggle call s:playground()

function s:install(args)
    delcommand TSInstall
    packadd nvim-treesitter
    exec 'TSInstall' a:args
endfunction
command! -nargs=1 TSInstall call s:install(<q-args>)
