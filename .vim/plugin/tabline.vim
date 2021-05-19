function! TabLine()
  let s = ''
  for i in range(1, tabpagenr('$'))
    let cur = i == tabpagenr()
    let h = cur ? 'TabLineSel' : 'TabLine'
    let ih = cur ? 'TabLineIndexSel' : 'TabLineIndex'
    let cwd = fnamemodify(getcwd(1, i), ':t')
    let s .= printf('%%#%s#%%%dT %%#%s#%d %%#%s#%s ', h, i, ih, i, h, cwd)
  endfor

  " After the last tab fill with TabLineFill and reset tab page nr
  let s .= '%#TabLineFill#%T'

  return s
endfunction

set tabline=%!TabLine()
