let s:cachedir = ''
let s:datadir = ''

function! vim#cachedir() abort
    if exists('*stdpath')
        return stdpath('cache')
    endif

    if empty(s:cachedir)
        let n = has('nvim') ? 'n' : ''
        if exists('$XDG_CACHE_HOME')
            let s:cachedir = $XDG_CACHE_HOME . '/' . n . 'vim'
        elseif has('unix')
            let s:cachedir = $HOME . '/.cache/' . n . 'vim'
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
        let n = has('nvim') ? 'n' : ''
        if exists('$XDG_DATA_HOME')
            let s:datadir = $XDG_DATA_HOME . '/' . n . 'vim'
        elseif has('unix')
            let s:datadir = $HOME . '/.local/share/' . n . 'vim'
        else
            let s:datadir = $HOME . '/vimfiles/cache'
        endif
    endif
    return s:datadir
endfunction

function! vim#mkdirs() abort
  if exists('*mkdir')
    let dirs = [&backupdir, &directory]
    if has('persistent_undo')
        let dirs += [&undodir]
    endif
    for dir in dirs
      silent! call mkdir(simplify(split(dir, ',')[0]), 'p')
    endfor
  endif
endfunction
