" Projections for vim runtime
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-20

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

let g:projectionist_heuristics = get(g:, 'projectionist_heuristics', {})

call extend(g:projectionist_heuristics, {
      \ 'vimrc': {
      \   'vimrc': { 'type': 'vimrc' },
      \   'plugin/*.vim': {
      \     'type': 'plugin',
      \     'template': ['" {}', '" Author: Greg Anders <greg@gpanders.com>', '" Date: {date}', '', 'if 0', "\tfinish", 'endif'],
      \     'alternate': 'after/plugin/{}.vim'
      \   },
      \   'after/plugin/*.vim': {
      \     'type': 'plugin',
      \     'template': ['" {}', '" Author: Greg Anders <greg@gpanders.com>', '" Date: {date}', '', 'if 0', "\tfinish", 'endif'],
      \     'alternate': 'plugin/{}.vim'
      \   },
      \   'ftplugin/*.vim': {
      \     'type': 'ftplugin',
      \     'template': ['" {} filetype plugin', '" Author: Greg Anders <greg@gpanders.com>', '', 'let b:undo_ftplugin = get(b:, ''undo_ftplugin'', '''')', '']
      \   },
      \   'after/ftplugin/*.vim': {
      \     'type': 'ftplugin',
      \     'template': ['" {} filetype plugin', '" Author: Greg Anders <greg@gpanders.com>', '', 'let b:undo_ftplugin = get(b:, ''undo_ftplugin'', '''')', '']
      \   },
      \   'compiler/*.vim': {
      \     'type': 'compiler',
      \   },
      \   'after/compiler/*.vim': {
      \     'type': 'compiler',
      \   }
      \ }})
