function! tabline#show() abort
  let s = ''
  for t in range(1, tabpagenr('$'))
    let hi = t == tabpagenr() ? 'TabLineSel' : 'TabLine'

    let s .= '%#'.hi.'#%'.t.'T'

    let bufnr = tabpagebuflist(t)[tabpagewinnr(t)-1]
    let modified = getbufvar(bufnr, '&modified')
    let cwd = pathshorten(fnamemodify(getcwd(-1, t), ':~'))
    let s .= ' '.t.(modified ? '+' : '').' '.cwd.' '
  endfor

  let s .= '%#TabLineFill#%T'

  return s
endfunction
