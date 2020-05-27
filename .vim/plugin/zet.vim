if exists('g:loaded_zet')
    finish
endif
let g:loaded_zet = 1

if empty($ZETTEL_DIR)
    if !empty($XDG_DATA_HOME)
        let $ZETTEL_DIR = $XDG_DATA_HOME . '/zet'
    else
        let $ZETTEL_DIR = $HOME . '/.local/share/zet'
    endif
endif

augroup plugin_zet
    autocmd!
    autocmd BufReadPost $ZETTEL_DIR/* setlocal include=\\[\\[\\zs\\d\\+\\ze]] includeexpr=glob($ZETTEL_DIR.'/'.v:fname.'*') omnifunc=zet#complete
augroup END
