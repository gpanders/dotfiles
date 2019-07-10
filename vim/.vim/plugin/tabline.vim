" Set the tabline
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-07-10

function! TabLine()
  let s = ''
  for i in range(tabpagenr('$'))
    " Set the highlighting
    if i +1 == tabpagenr()
      let s .= '%#TabLineSel#'
    else
      let s .= '%#TabLine#'
    endif

    " Set the tab page number
    let s .= '%' . (i + 1) . 'T'

    " Set the tab label
    let s .= ' ' . pathshorten(fnamemodify(getcwd(-1, i + 1), ':~:.')) . ' '
  endfor

  " After the last tab fill with TabLineFill and reset tab page nr
  let s .= '%#TabLineFill#%T'

  return s
endfunction

set tabline=%!TabLine()
