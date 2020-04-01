" Set the tabline

function! TabLine()
  let s = ''
  for i in range(1, tabpagenr('$'))
    " Set the highlighting
    if i == tabpagenr()
      let s .= '%#TabLineSel#'
    else
      let s .= '%#TabLine#'
    endif

    " Set the tab page number
    let s .= '%' . i . 'T'

    " Set the tab label
    let cwd = fnamemodify(getcwd(1, i), ':~')
    let s .= ' ' . i . ':' . pathshorten(cwd) . ' '
  endfor

  " After the last tab fill with TabLineFill and reset tab page nr
  let s .= '%#TabLineFill#%T'

  return s
endfunction

set tabline=%!TabLine()
