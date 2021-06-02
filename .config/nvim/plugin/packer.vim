function! s:update()
    lua require('plugins')
    PackerUpdate
endfunction

command PackerUpdate call s:update()
