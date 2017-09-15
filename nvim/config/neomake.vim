if exists('plugs') && has_key(plugs, 'neomake')
  if has('mac')
    let g:neomake_c_clangtidy_maker = neomake#makers#ft#c#clangtidy()
    let g:neomake_c_clangtidy_maker.exe = '/usr/local/opt/llvm/bin/clang-tidy'
    let g:neomake_cpp_clangtidy_maker = g:neomake_c_clangtidy_maker

    let g:neomake_c_clangcheck_maker = neomake#makers#ft#c#clangcheck()
    let g:neomake_c_clangcheck_maker.exe = '/usr/local/opt/llvm/bin/clang-check'
    let g:neomake_cpp_clangcheck_maker = g:neomake_c_clangcheck_maker
  endif

  " Disable clang as a maker because you cannot specify a compilation database,
  " so it always complains about things it shouldn't
  let g:neomake_cpp_enabled_makers = neomake#makers#ft#cpp#EnabledMakers()
  let idx = index(g:neomake_cpp_enabled_makers, 'clang')
  if idx >= 0
    call remove(g:neomake_cpp_enabled_makers, idx)
  endif

  " Run Neomake on read and write operations
  autocmd! BufReadPost,BufWritePost * Neomake
endif
