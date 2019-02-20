" Run some normal-mode keystrokes without jumping around
function! vimrc#Anchor(keys) abort
  let l:view = winsaveview()
  execute 'normal! '.a:keys
  call winrestview(l:view)
endfunction
