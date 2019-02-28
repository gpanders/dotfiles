if !exists('g:loaded_projectionist')
  finish
endif

let g:projectionist_heuristics ={
      \ 'Cargo.toml&src/': {
      \   'src/*.rs': {'type': 'src'},
      \   'Cargo.toml': {'type': 'toml' }
      \ },
      \ 'vimrc': {
      \   'vimrc': { 'type': 'vimrc' },
      \   'plugin/*.vim': {
      \     'type': 'plugin',
      \     'template': ['" {} configuration', '" Author: Greg Anders', '', 'if <condition>', "\tfinish", 'endif']
      \   },
      \   'after/plugin/*.vim': {
      \     'type': 'plugin',
      \     'template': ['" {}', '" Author: Greg Anders', '', 'if <condition>', "\tfinish", 'endif']
      \   },
      \   'ftplugin/*.vim': {
      \     'type': 'ftplugin',
      \     'template': ['" {} filetype plugin', '" Author: Greg Anders', '', 'if &filetype !=# ''{}''', "\tfinish", 'endif']
      \   },
      \   'after/ftplugin/*.vim': {
      \     'type': 'ftplugin',
      \     'template': ['" {} filetype plugin', '" Author: Greg Anders', '', 'if &filetype !=# ''{}''', "\tfinish", 'endif']
      \   },
      \   'compiler/*.vim': {
      \     'type': 'compiler',
      \   },
      \   'after/compiler/*.vim': {
      \     'type': 'compiler',
      \   }
      \ }}
