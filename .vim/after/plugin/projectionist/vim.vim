if !get(g:, 'loaded_projectionist', 0)
  finish
endif

let g:projectionist_heuristics = get(g:, 'projectionist_heuristics', {})

let s:plugin_template = ['" {}', '" Author: Greg Anders <greg@gpanders.com>', '" Date: {date}', '', 'if 0', "\tfinish", 'endif']
let s:ftplugin_template = ['" {} filetype plugin', '" Author: Greg Anders <greg@gpanders.com>', '', 'let b:undo_ftplugin = get(b:, ''undo_ftplugin'', '''')', '']

call extend(g:projectionist_heuristics, {
      \ 'vimrc': {
      \   'plugin/*.vim': { 'template': s:plugin_template, },
      \   'after/plugin/*.vim': { 'template': s:plugin_template, },
      \   'ftplugin/*.vim': { 'template': s:ftplugin_template, },
      \   'after/ftplugin/*.vim': { 'template': s:ftplugin_template },
      \ }})
