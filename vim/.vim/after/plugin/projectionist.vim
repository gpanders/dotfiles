" vim-projectionist configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-01-04

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

if !exists('g:projectionist_transformations')
  let g:projectionist_transformations = {}
endif

function! g:projectionist_transformations.date(input, o) abort
  return strftime('%Y-%m-%d')
endfunction

let g:projectionist_heuristics = {
      \ 'Cargo.toml&src/': {
      \   'src/*.rs': {'type': 'src'},
      \   'Cargo.toml': {'type': 'toml' }
      \ },
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
      \     'template': ['" {} filetype plugin', '" Author: Greg Anders <greg@gpanders.com>', '', 'if &filetype !=# ''{}''', "\tfinish", 'endif']
      \   },
      \   'after/ftplugin/*.vim': {
      \     'type': 'ftplugin',
      \     'template': ['" {} filetype plugin', '" Author: Greg Anders <greg@gpanders.com>', '', 'if &filetype !=# ''{}''', "\tfinish", 'endif']
      \   },
      \   'compiler/*.vim': {
      \     'type': 'compiler',
      \   },
      \   'after/compiler/*.vim': {
      \     'type': 'compiler',
      \   }
      \ },
      \ 'CMakeLists.txt&build/': {
      \   '*': {
      \     'make': 'cmake -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .',
      \     'dispatch': '-dir=build make',
      \   },
      \   'CMakeLists.txt': { 'type': 'list' },
      \   'src/*.cc': {
      \     'type': 'src',
      \     'alternate': ['include/{}.h', 'src/{}.h'],
      \   },
      \   'src/*.cpp': {
      \     'type': 'src',
      \     'alternate': ['include/{}.h', 'src/{}.h'],
      \   },
      \   'src/*.h': {
      \     'type': 'header',
      \     'alternate': ['src/{}.cc', 'src/{}.cpp'],
      \     'template': [
      \       '#ifndef _{project|basename|snakecase|uppercase}_{snakecase|uppercase}_H_',
      \       '#define _{project|basename|snakecase|uppercase}_{snakecase|uppercase}_H_',
      \       '',
      \       '#endif //_{project|basename|snakecase|uppercase}_{snakecase|uppercase}_H_',
      \     ],
      \   },
      \   'include/*.h': {
      \     'type': 'header',
      \     'alternate': ['src/{}.cc', 'src/{}.cpp'],
      \     'template': [
      \       '#ifndef _{project|basename|snakecase|uppercase}_{uppercase}_H_',
      \       '#define _{project|basename|snakecase|uppercase}_{uppercase}_H_',
      \       '',
      \       '#endif //_{project|basename|snakecase|uppercase}_{uppercase}_H_',
      \     ],
      \   },
      \   'README.md': { 'type': 'doc' },
      \ },
      \ 'include/': {
      \   '*.h':   { 'path': 'include' },
      \   '*.hpp': { 'path': 'include' },
      \   '*.c':   { 'path': 'include' },
      \   '*.cc':  { 'path': 'include' },
      \   '*.cpp': { 'path': 'include' },
      \ }}
