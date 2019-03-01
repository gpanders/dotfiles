" ultisnips configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-02-27

if !get(g:, 'did_plugin_ultisnips', 0)
  finish
endif

let g:UltiSnipsEnableSnipMate = 0
let g:UltiSnipsEditSplit = 'tabdo'
let g:UltiSnipsSnippetsDir = $MYVIMRUNTIME . '/snips'
let g:UltiSnipsSnippetDirectories = ['snips']
