if exists('b:did_ftplugin')
    finish
endif

runtime! ftplugin/tcl.vim ftplugin/tcl_*.vim ftplugin/tcl/*.vim
