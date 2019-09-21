function! vim#cachedir()
  let n = has('nvim') ? 'n' : ''
  if exists('$XDG_CACHE_HOME')
    return $XDG_CACHE_HOME . '/' . n . 'vim'
  elseif has('unix')
    return $HOME . '/.cache/' . n . 'vim'
  else
    return . '/vimfiles/cache'
  endif
endfunction

function! vim#datadir()
  let n = has('nvim') ? 'n' : ''
  if exists('$XDG_DATA_HOME')
    return $XDG_DATA_HOME . '/' . n . 'vim'
  elseif has('unix')
    return $HOME . '/.local/share/' . n . 'vim'
  else
    return . '/vimfiles/cache'
  endif
endfunction
