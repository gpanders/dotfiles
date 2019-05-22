let s:mark_debug_def = 'attribute MARK_DEBUG : string;'
let s:mark_debug = 'attribute MARK_DEBUG of {} : signal is "true";'

function! ft#vhdl#toggle_debug()
  " Use a mark to preserve current cursor location, since it will
  " automatically update its line number as new lines are added
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

    " Jump to signal definition, if it exists
    if !search('\c^\s*signal\s\+' . signal . '\s*:')
      echohl ErrorMsg
      echo 'No signal ' . signal . ' found'
      echohl None
      return
    endif
  endif

  let text = substitute(s:mark_debug, '{}', signal, '')

  " If text already exists, delete it
  let textloc = search('^\s*' . text, 'n')
  if textloc
    execute textloc . 'd'
  else
    " Use :normal! o to take advantage of Vim's autoindenting
    execute 'normal! o' . text . "\<Esc>"
  endif

  " If MARK_DEBUG attribute is not defined, define it
  if !search(s:mark_debug_def, 'n')
    let archbegin = search('^\s*architecture\s\+')
    if archbegin
      execute "normal! o\<CR>" . s:mark_debug_def . "\<Esc>"
    endif
  endif

  " Reposition the cursor to the mark set at the beginning of the function
  let view.lnum = line("'z")
  call winrestview(view)

  " Restore the old value of the mark
  call setpos("'z", oldz)
endfunction
