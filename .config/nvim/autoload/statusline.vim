function! statusline#git() abort
  if exists('b:gitsigns_head')
    return b:gitsigns_head
  end

  if exists('*FugitiveHead')
    let branch = FugitiveHead()
    if branch !=# ''
      return branch
    endif
  endif
  return ''
endfunction

function! statusline#obsession() abort
  if exists('*ObsessionStatus')
    let s = ObsessionStatus()
    if !empty(s)
      let s .= ' '
    endif
    return s
  endif
  return ''
endfunction
