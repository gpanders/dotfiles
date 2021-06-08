function! s:invalid(opt, val) abort
    echom printf('Invalid value for option %s: %s', a:opt, a:val)
endfunction

" Modified version of glob2regpat that does not match path separators on *.
" Basically, this replaces single instances of * with the regex pattern [^/]*.
" However, the star in the replacement pattern also gets interpreted by
" glob2regpat, so we insert a placeholder, pass it through glob2regpat, then
" replace the placeholder with the actual regex pattern
function! s:glob2regpat(glob) abort
    let g = substitute(a:glob, '\*\@<!\*\*\@!', '@@PLACEHOLDER@@', 'g')
    return substitute(glob2regpat(g), '@@PLACEHOLDER@@', '[^/]*', 'g')
endfunction

function! s:apply(opts) abort
    for opt in keys(a:opts)
        let val = a:opts[opt]
        if opt ==# 'end_of_line'
            if val ==# 'lf'
                setlocal fileformat=unix
            elseif val ==# 'crlf'
                setlocal fileformat=dos
            else
                call s:invalid(opt, val)
            endif
        elseif opt ==# 'indent_style'
            if val ==# 'tab'
                setlocal noexpandtab
                if !has_key(a:opts, 'indent_size')
                    setlocal shiftwidth=0
                endif
            elseif val ==# 'space'
                setlocal expandtab
            else
                call s:invalid(opt, val)
            endif
        elseif opt ==# 'indent_size'
            let n = str2nr(val)
            if n != 0
                let &l:shiftwidth = n
            else
                call s:invalid(opt, val)
            endif
        elseif opt ==# 'tab_width'
            let n = str2nr(val)
            if n != 0
                let &l:tabstop = n
            else
                call s:invalid(opt, val)
            endif
        elseif opt ==# 'max_line_length'
            let n = str2nr(val)
            if n != 0
                let &l:textwidth = n
            else
                call s:invalid(opt, val)
            endif
        elseif opt ==# 'trim_trailing_whitespace'
            if val ==# 'true' && exists(':StripTrailingWhitespace') == 2
                autocmd editorconfig BufWritePre <buffer> silent StripTrailingWhitespace
            endif
        elseif opt ==# 'insert_final_newline'
            if val ==# 'false'
                setlocal nofixendofline
            elseif val ==# 'true'
                setlocal fixendofline
            else
                call s:invalid(opt, val)
            endif
        endif
    endfor
endfunction

" Parse 'config' and return a Dict of all options that match 'filepath'
function! s:parse(filepath, config) abort
    let pat = ''
    let opts = {}
    let confdir = fnamemodify(a:config, ':h')
    for line in readfile(a:config)
        if line =~# '^\s*$' || line =~# '^\s*[#;]'
            continue
        endif

        if line =~# '^\['
            let glob = matchstr(line, '^\s*\[\zs\%([^#;]\|\\#\|\\;\)\+\ze]')
            if glob =~# '/'
                if glob[0] ==# '/'
                    let glob = glob[1:]
                endif
                let glob = confdir . '/' . glob
            else
                let glob = '**/' . glob
            endif
            let pat = s:glob2regpat(glob)
            continue
        endif

        let m = matchlist(line, '^\s*\([^:=\s][^:=]\{-}\)\s*[:=]\s*\(.\{-}\)\s*$')
        if empty(m)
            continue
        endif

        let [opt, val] = [m[1], tolower(m[2])]

        if empty(pat) && opt ==# 'root'
            let opts.root = val ==# 'true'
            continue
        endif

        if a:filepath !~# pat
            continue
        endif

        let opts[opt] = val
    endfor

    return opts
endfunction

function! editorconfig#config() abort
    if !empty(&buftype) || !&modifiable
	return
    endif

    let path = expand('%:p')
    if empty(path)
        return
    endif

    let opts = {}
    let curdir = fnamemodify(path, ':h')
    while 1
        let config = curdir . '/.editorconfig'
        if filereadable(config)
            call extend(opts, s:parse(path, config), 'keep')
            if get(opts, 'root')
                break
            endif
        endif

        let parent = fnamemodify(curdir, ':h')
        if parent ==# curdir
            break
        endif
        let curdir = parent
    endwhile

    call s:apply(opts)
endfunction
