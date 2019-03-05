" Keep v:oldfiles up to date as I use vim
" Author: Greg Anders <greg@gpanders.com>
" License: Same as vim itself

function! s:UpdateOldfiles()
  let fname = expand('%:p:~')
  let idx = index(v:oldfiles, fname)
  if idx > -1
    call remove(v:oldfiles, idx)
  endif
  call insert(v:oldfiles, fname)
endfunction

augroup oldfiles
  autocmd!
  autocmd BufRead * call s:UpdateOldfiles()
augroup END
