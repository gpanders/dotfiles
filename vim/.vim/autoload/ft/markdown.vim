function! ft#markdown#eval()
  let view = winsaveview()
  let [line, col] = [line('.'), col('.')]
  let start = search('^\s*`\{3,}\%(\S\+\)\=\s*$', 'bnW')
  if !start
    return
  endif

  call cursor(start, 1)
  let ticks = matchstr(getline(start), '`\{3,}')
  let lang = matchstr(getline(start), '`\{3,}\zs\S\+')
  let end = search('^\s*' . ticks . '\s*$', 'nW')
  let langidx = index(map(copy(g:markdown_interp_languages), 'split(v:val, "=")[0]'), lang)

  if end < line || langidx < 0
    call winrestview(view)
    return
  endif

  if g:markdown_interp_languages[langidx] !=# lang
    let lang = split(g:markdown_interp_languages[langidx], '=')[1]
  endif

  let block = getline(start + 1, end - 1)
  let tmp = tempname()
  call writefile(block, tmp)
  echo system(lang . ' ' . tmp)
  call winrestview(view)
endfunction
