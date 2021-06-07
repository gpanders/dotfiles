function! statusline#git() abort
  if get(g:, 'loaded_fugitive')
    let branch = fugitive#head()
    if branch !=# ''
      return branch
    endif
  endif
  return ''
endfunction
