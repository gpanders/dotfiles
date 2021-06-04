function! ft#go#fmt() abort
    let view = winsaveview()
    keepjumps %!gofmt
    call winrestview(view)
endfunction
