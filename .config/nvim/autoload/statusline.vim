function! statusline#git() abort
  if exists('b:gitsigns_head')
    return b:gitsigns_head
  end

  if get(g:, 'loaded_fugitive')
    let branch = fugitive#head()
    if branch !=# ''
      return branch
    endif
  endif
  return ''
endfunction

function! statusline#obsession() abort
  if get(g:, 'loaded_obsession')
    let s = ObsessionStatus()
    if !empty(s)
      let s .= ' '
    endif
    return s
  endif
  return ''
endfunction
