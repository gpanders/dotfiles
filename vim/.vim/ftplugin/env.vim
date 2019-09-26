" env filetype plugin
" Author: Greg Anders <greg@gpanders.com>

if exists('b:did_ftplugin')
    finish
endif

runtime! ftplugin/sh.vim ftplugin/sh_*.vim ftplugin/sh/*.vim
