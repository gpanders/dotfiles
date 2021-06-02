setlocal commentstring=//%s
setlocal define&
setlocal include=^\\s*#\\s*include\\s*[\"<]\\@=
setlocal includeexpr&
setlocal textwidth=80

" Support /// as a comment leader, used for writing Doxygen comments
setlocal comments-=://
setlocal comments+=:///,://

call ft#c#set_path()
call ft#c#tags(v:false)

augroup ftplugin_c
  autocmd!
  autocmd BufWritePost <buffer> call ft#c#tags(v:true)
augroup END

let b:undo_ftplugin .= '|setl cms< def< inc< inex< path< tw< com<|au! ftplugin_c'

if exists(':Man') == 2
  setlocal keywordprg=:Man
  let b:undo_ftplugin .= '|setl kp<'
endif

if executable('clang-format')
  setlocal formatprg=clang-format\ -style=file\ -fallback-style=none
  let b:undo_ftplugin .= '|setl fp<'
endif
