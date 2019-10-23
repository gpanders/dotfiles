function! ft#markdown#formatexpr()
    " Only use this in normal mode to prevent automatic formatting being
    " messed up in insert mode
    if mode() !=# 'n'
        return 1
    endif

    let lastsearch = @/
    let range = v:lnum . ',' . (v:lnum + v:count - 1)
    exe range . '!' . &l:formatprg
    " Join wrapped links
    silent exe range . 'g/\[[^]]*$/.,/\%(\[.*\)\@<!]/j'
    " Convert *italics* (used by pandoc) to _italics_ (which I prefer)
    exe range . 's/\*\([^*]\+\)\*/_\1_/ge'
    " Convert {.language} to just language
    exe range . 's/[~`]\{3,}\zs\s*{\.\(\w\+\)}/\1/g'
    let @/ = lastsearch
endfunction

function! ft#markdown#toc()
    let fp = &l:formatprg
    let &l:formatprg .= ' --toc'
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
  let start = search('^\s*[`~]\{3,}\s*\%({\.\)\?\a\+}\?\s*$', 'bnW')
  if !start
    return
  endif

  call cursor(start, 1)
  let [fence, lang] = matchlist(getline(start), '\([`~]\{3,}\)\s*\%({\.\)\?\(\a\+\)\?')[1:2]
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
