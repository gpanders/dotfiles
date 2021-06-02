if !get(g:, 'loaded_easy_align_plugin')
  finish
endif

" Pattern to match Markdown table header rows
let s:headerpat =  '|\zs\( \?\)\s\{-}\(:\?\)\([+=-]\)[+=-]*\(:\?\)\s\{-}\ze|'

nmap gl <Plug>(EasyAlign)
xmap gl <Plug>(EasyAlign)

" Source: https://gist.github.com/tpope/287147
inoremap <silent> <Bar> <Bar><Esc>:call <SID>align()<CR>a

" Normalize table header rows
" Table headers can have an optional left or right characters to set cell
" alignment. If both characters are present, cell is center aligned.
" Examples:
"
"   | ---      |    ->  | -------- |
"   |:------    |   ->  |:---------|
"   | :---:   |     ->  | :-----: |
function! s:normalize()
    " Entire matched contents between the pipe characters
    let contents = submatch(0)

    " Padding around delimiter characters
    let pad = submatch(1)

    " Optional character indicating cell should be left-aligned
    let lalign = submatch(2)

    " Header delimiter (e.g. -, =, or +)
    let delim = submatch(3)

    " Optional character indicating cell should be right-aligned
    let ralign = submatch(4)

    " Number of delimeters to fill into header
    let nchars = strlen(contents) - strlen(lalign . ralign) - 2*strlen(pad)
    let str = repeat(delim, nchars)
    return pad . lalign . str . ralign . pad
endfunction

" Align tables along pipe characters
function! s:align()
  let [_, lnum, col, _, _] = getcurpos()
  let line = getline('.')

  " Check if current line contains a pipe and either the next or previous line
  " contains a pipe
  let p = '^\s*|\s.*\s|\s*$'
  if line =~# '^\s*|' && (getline(lnum - 1) =~# p || getline(lnum + 1) =~# p)
    let cur = line[0:col]

    " Get table column
    let column = strlen(substitute(cur, '[^|]', '', 'g'))

    " Position of cursor relative to the current table column
    let position = strlen(matchstr(cur, '.*|\s*\zs.*'))

    " Align current paragraph along pipes
    '{,'}EasyAlign *|

    " Normalize header rows
    for l in range(line('''{'), line('''}'))
        call setline(l, substitute(getline(l), s:headerpat, '\=s:normalize()', 'g'))
    endfor

    " Move cursor back to original position
    call cursor(lnum, 0)
    call search(repeat('[^|]*|', column) . '\s\{-\}' . repeat('.', position), 'ce', lnum)
  endif
endfunction
