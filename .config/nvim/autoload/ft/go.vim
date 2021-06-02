function! ft#go#fmt() abort
    let view = winsaveview()
    %!gofmt
    call winrestview(view)
endfunction
