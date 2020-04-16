let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

" Z! execute line as shell command
" Inspired by the Shdo buffers from vim-dirvish
nnoremap <buffer> Z! ^"zyg_:!<C-R>z<CR>
let b:undo_ftplugin .= '|nun <buffer> Z!'

setlocal textwidth=79
setlocal formatoptions-=t
let b:undo_ftplugin .= '|setl tw< fo<'

if executable('shellcheck')
    compiler shellcheck
endif

if executable('shfmt')
    setlocal formatprg=shfmt\ -s
    if get(b:, 'is_bash')
        setlocal formatprg+=\ -ln=bash
    else
        setlocal formatprg+=\ -ln=posix
    endif
    setlocal formatprg+=\ -
    let b:undo_ftplugin .= '|setl fp<'
endif
