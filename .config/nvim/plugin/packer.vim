function! s:update()
    lua require('plugins')
    PackerUpdate
endfunction

function! s:clean()
    lua require('plugins')
    PackerClean
endfunction

command PackerUpdate call s:update()
command PackerClean call s:clean()
