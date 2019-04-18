" Projections for cmake projects
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-20

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

let g:projectionist_heuristics = get(g:, 'projectionist_heuristics', {})

call extend(g:projectionist_heuristics, {
      \ 'CMakeLists.txt&build/|CMakeLists.txt&src/': {
      \   '*': {
      \     'make': 'cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .',
      \     'dispatch': '-dir=build make',
      \   },
      \   'CMakeLists.txt': { 'type': 'list' },
      \   'src/*.cc': {
      \     'type': 'src',
      \     'alternate': ['include/{}.h', 'src/{}.h', 'test/test_{}.cc'],
      \   },
      \   'src/*.cpp': {
      \     'type': 'src',
      \     'alternate': ['include/{}.h', 'src/{}.h', 'test/test_{}.cpp'],
      \   },
      \   'src/*.h': {
      \     'type': 'include',
      \     'alternate': ['src/{}.cc', 'src/{}.cpp', 'test/test_{}.cc', 'test/test_{}.cpp'],
      \     'template': [
      \       '#ifndef _{project|basename|snakecase|uppercase}_{snakecase|uppercase}_H_',
      \       '#define _{project|basename|snakecase|uppercase}_{snakecase|uppercase}_H_',
      \       '',
      \       '#endif  //_{project|basename|snakecase|uppercase}_{snakecase|uppercase}_H_',
      \     ],
      \   },
      \   'include/*.h': {
      \     'type': 'include',
      \     'alternate': ['src/{}.cc', 'src/{}.cpp', 'test/test_{}.cc', 'test/test_{}.cpp'],
      \     'template': [
      \       '#ifndef _{project|basename|snakecase|uppercase}_{uppercase}_H_',
      \       '#define _{project|basename|snakecase|uppercase}_{uppercase}_H_',
      \       '',
      \       '#endif  //_{project|basename|snakecase|uppercase}_{uppercase}_H_',
      \     ],
      \   },
      \   'README.md': { 'type': 'doc' },
      \ }})
