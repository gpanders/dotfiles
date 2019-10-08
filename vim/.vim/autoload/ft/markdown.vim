function! ft#markdown#format()
    " Only use this in normal mode to prevent automatic formatting being
    " messed up in insert mode
    if mode() !=# 'n'
        return 1
    endif

    " Escape whitespace characters in link text in lists and then run
    " 'formatprg' as usual
    let start = v:lnum
    let end = start + v:count - 1
    exe start . ',' . end . 's/\%(^\s*\%([-*]\|[0-9]\+\.\)\s\+\[.*\)\@<=\s\ze.*]/\\&/g'
    exe start . ',' . end . '!' . &l:formatprg
endfunction

function! ft#markdown#toc()
    let fp = &l:formatprg
    let &l:formatprg = fp . ' --toc'
    let view = winsaveview()
    normal! gggqG
    call winrestview(view)
    let &l:formatprg = fp
endfunction

function! ft#markdown#reflinks(bang)
    if a:bang
        if &l:formatprg =~# '--reference-links'
            let &l:formatprg = substitute(&l:formatprg, ' --reference-links', '', '')
        endif
    else
        if &l:formatprg !~# '--reference-links'
            let &l:formatprg .= ' --reference-links'
        endif
    endif
    let view = winsaveview()
    normal! gggqG
    call winrestview(view)
endfunction

function! ft#markdown#section(back)
    for _ in range(v:count1)
        call search('\%(^#\+ \|^\S.*\n^-\+$\|^\S.*\n^=\+$\)',
                    \ (a:back ? 'b' : '') . 'sW')
    endfor
endfunction

function! ft#markdown#eval()
  let view = winsaveview()
  let line = line('.')
  let start = search('^\s*[`~]\{3,}\S*\s*$', 'bnW')
  if !start
    return
  endif

  call cursor(start, 1)
  let [fence, lang] = matchlist(getline(start), '\([`~]\{3,}\)\(\S\+\)\?')[1:2]
  let end = search('^\s*' . fence . '\s*$', 'nW')
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
