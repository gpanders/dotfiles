function! s:filter(val) abort
  if !filereadable(a:val)
    return 0
  endif

  if !exists('g:oldfiles_blacklist') || type(g:oldfiles_blacklist) != type([])
    return 1
  endif

  for pat in g:oldfiles_blacklist
    if a:val =~# pat
      return 0
    endif
  endfor

  return 1
endfunction

function! oldfiles#textfunc(d) abort
  let items = getqflist({'id': a:d.id, 'items': v:true}).items
  let l = []
  for i in range(a:d.start_idx - 1, a:d.end_idx - 1)
    let item = items[i]
    let fname = fnamemodify(bufname(item.bufnr), ':p')
    call add(l, item.nr . ': ' . fname)
  endfor
  return l
endfunction

function! oldfiles#add() abort
  " Add file to oldfiles when opened
  let fname = expand('<afile>:p')
  if empty(fname) || !filereadable(fname) || !&buflisted
    return
  endif

  let v:oldfiles = [fname] + filter(v:oldfiles, {_, f -> f !=# fname})
endfunction

function! oldfiles#remove() abort
  " Remove file from oldfiles if it is no longer valid
  let fname = expand('<afile>:p')
  if empty(fname) || filereadable(fname)
    return
  endif

  call filter(v:oldfiles, {_, f -> f !=# fname})
endfunction

function! oldfiles#open(bang, mods, ...) abort
  silent doautocmd QuickFixCmdPre Oldfiles
  let pat = a:0 ? a:1 : '//'
  let cmd = 'filter' . (a:bang ? '! ' : ' ') . pat . ' oldfiles'
  let oldfiles = split(execute(cmd), '\n')
  call filter(oldfiles, 's:filter(expand(split(v:val, ''^\d\+\zs:\s\+'')[1]))')
  call setqflist([], ' ', {'lines': oldfiles, 'efm': '%n: %f', 'title': ':Oldfiles', 'quickfixtextfunc': 'oldfiles#textfunc'})
  silent doautocmd QuickFixCmdPost Oldfiles
  exe a:mods 'copen'
endfunction
