if executable('fish_indent')
    setlocal formatprg=fish_indent
    augroup ftplugin_fish
        autocmd!
        autocmd BufWritePre <buffer>
                    \ let w:view = winsaveview() |
                    \ exec 'keepjumps %!fish_indent' |
                    \ call winrestview(w:view) |
                    \ unlet! w:view
    augroup END

    let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<|au! ftplugin_fish BufWritePre <buffer>'
endif
