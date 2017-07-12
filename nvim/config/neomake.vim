let g:neomake_cpp_enabled_makers = ['clangtidy']
let g:neomake_cpp_clangtidy_maker = {
            \ 'exe': '/usr/local/bin/clang-tidy',
            \ 'args': ['-checks=*'],
            \ }

" Run Neomake on read and write operations
autocmd! BufReadPost,BufWritePost * Neomake
