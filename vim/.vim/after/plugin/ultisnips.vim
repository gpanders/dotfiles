" ultisnips configuration
" Author: Greg Anders

if !get(g:, 'did_plugin_ultisnips', 0)
  finish
endif

let g:UltiSnipsEnableSnipMate = 0
let g:UltiSnipsEditSplit = 'tabdo'
let g:UltiSnipsSnippetsDir = $MYVIMRUNTIME . '/snips'
let g:UltiSnipsSnippetDirectories = ['snips']
