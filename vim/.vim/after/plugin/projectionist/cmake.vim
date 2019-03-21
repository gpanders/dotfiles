" Projections for cmake projects
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-20

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

let g:projectionist_heuristics = get(g:, 'projectionist_heuristics', {})

call extend(g:projectionist_heuristics, {
      \ 'CMakeLists.txt&build/': {
      \   '*': {
      \     'dispatch': 'cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .',
      \     'make': 'cmake --build build',
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
      \ }})
