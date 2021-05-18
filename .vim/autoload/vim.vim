let s:cachedir = ''
let s:datadir = ''

function! vim#cachedir() abort
    if exists('*stdpath')
        return stdpath('cache')
    endif

    if empty(s:cachedir)
        if exists('$XDG_CACHE_HOME')
            let s:cachedir = $XDG_CACHE_HOME . '/vim'
        elseif has('unix')
            let s:cachedir = $HOME . '/.cache/vim'
        else
            let s:cachedir = $HOME . '/vimfiles/cache'
        endif
    endif
    return s:cachedir
endfunction

function! vim#datadir() abort
    if exists('*stdpath')
        return stdpath('data')
    endif

    if empty(s:datadir)
        if exists('$XDG_DATA_HOME')
            let s:datadir = $XDG_DATA_HOME . '/vim'
        elseif has('unix')
            let s:datadir = $HOME . '/.local/share/vim'
        else
            let s:datadir = $HOME . '/vimfiles/cache'
        endif
    endif
    return s:datadir
endfunction

function! vim#mkdirs() abort
    for dir in [&backupdir, &directory, &undodir]
      call mkdir(fnamemodify(split(dir, ',')[0], 'p'), 'p')
    endfor
endfunction
