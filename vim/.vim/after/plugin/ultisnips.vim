" ultisnips configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-02-27

if !get(g:, 'did_plugin_ultisnips')
  finish
endif

let g:UltiSnipsEnableSnipMate = 0
let g:UltiSnipsEditSplit = 'tabdo'
let g:UltiSnipsSnippetsDir = $VIMHOME . '/snips'
let g:UltiSnipsSnippetDirectories = ['snips']
