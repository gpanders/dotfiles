function! s:invalid(opt, val) abort
    echom printf('Invalid value for option %s: %s', a:opt, a:val)
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
                autocmd editorconfig BufWritePre <buffer> StripTrailingWhitespace
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

function! s:parse(fname, config) abort
    let pat = ''
    let opts = {}
    for line in readfile(a:config)
        if empty(line) || line =~# '^\s*#'
            continue
        endif

        if line =~# '^\['
            let pat = glob2regpat(matchstr(line, '^\[\zs.\+\ze\]$'))
            continue
        endif

        let m = matchlist(line, '^\(\w\+\)\s*=\s*\(.\+\)$')
        if empty(m)
            continue
        endif

        let [opt, val] = m[1:2]

        if opt ==# 'root' && val ==# 'true'
            let opts.root = 1
            continue
        endif

        if a:fname !~# pat
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

    let fname = expand('%:p:.')
    if empty(fname)
        return
    endif

    let opts = {}
    let cwd = expand('%:p:h')
    while cwd !=# '/'
        let config = cwd . '/.editorconfig'
        if filereadable(config)
            call extend(opts, s:parse(fname, config), 'keep')
            if get(opts, 'root')
                break
            endif
        endif
        let cwd = fnamemodify(cwd, ':h')
    endwhile

    call s:apply(opts)
endfunction
