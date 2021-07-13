function! ft#lua#format() abort
    if mode() ==# 'i' || mode() ==# 'R'
        return 0
    endif

    let opts = '--column-width ' . &l:textwidth
    let opts .= ' --indent-type ' . (&l:expandtab ? 'spaces' : 'tabs')
    let opts .= ' --indent-width ' . &l:shiftwidth
    let opts .= ' --line-endings ' . (&l:fileformat ==# 'dos' ? 'windows' : 'unix')

    let start = v:lnum
    let end = v:lnum + v:count - 1

    exec start.','.end.'!stylua '.opts. ' -'
endfunction
