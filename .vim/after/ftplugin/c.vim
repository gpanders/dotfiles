" Set comment string
setlocal commentstring=//%s

" Set include and define patterns
setlocal define&
setlocal include=^\\s*#\\s*include\\s*[\"<]\\@=
setlocal includeexpr&

" Keep lines at 80 characters or fewer
setlocal textwidth=80

" Support /// as a comment leader, used for writing Doxygen comments
setlocal comments-=://
setlocal comments+=:///,://

" Set path
call ft#c#set_path()

" Create and set tags file
call ft#c#tags(v:false)

augroup ftplugin_c
  autocmd!
  autocmd BufWritePost <buffer> call ft#c#tags(v:true)
augroup END

let b:undo_ftplugin .= '|setl cms< def< inc< inex< path< tw< com<|au! ftplugin_c'

" Use improved :Man command as keywordprg
if exists(':Man') == 2
  setlocal keywordprg=:Man
  let b:undo_ftplugin .= '|setl kp<'
endif

if executable('clang-format')
  setlocal formatprg=clang-format\ -style=file\ -fallback-style=none
  let b:undo_ftplugin .= '|setl fp<'
endif
