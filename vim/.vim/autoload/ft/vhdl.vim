let s:mark_debug_def = 'attribute MARK_DEBUG : string;'
let s:mark_debug = 'attribute MARK_DEBUG of {} : signal is "true";'

function! ft#vhdl#toggle_debug()
  let oldz = getpos("'z")
  mark z
  let l = getline('.')
  let view = winsaveview()
  let matches = matchlist(l, '\c^\s*signal\s\+\(\h\w*\)\s*:')
  if !empty(matches)
    " The current line is a signal definition
    let signal = matches[1]
  else
    let signal = expand('<cword>')
    let pos = search('\c^\s*signal\s\+' . signal . '\s*:')
    if !pos
      echohl ErrorMsg
      echo 'No signal ' . signal . ' found'
      echohl None
      return
    endif
  endif

  let text = substitute(s:mark_debug, '{}', signal, '')
  " If text already exists, delete it
  let attr = search('^\s*' . text, 'n')
  if attr
    execute attr . 'd'
  else
    execute 'normal! o' . text . "\<Esc>"
  endif

  " If MARK_DEBUG attribute is not defined, define it
  if !search(s:mark_debug_def, 'n')
    let archbegin = search('^\s*architecture\s\+', 'n')
    if archbegin
      call cursor(archbegin, 1)
      execute "normal! o\<CR>" . s:mark_debug_def . "\<Esc>"
    endif
  endif
  let view.lnum = line("'z")
  call winrestview(view)
  call setpos("'z", oldz)
endfunction
