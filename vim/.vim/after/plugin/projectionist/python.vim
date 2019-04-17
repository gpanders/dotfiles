" Projections for projects with compile_commands.json
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-20

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

let g:projectionist_heuristics = get(g:, 'projectionist_heuristics', {})

call extend(g:projectionist_heuristics, {
      \ 'setup.py': {
      \   'setup.py': {'type': 'setup'},
      \   '*.py': {
      \     'path': '**',
      \     'alternate': ['test_{basename}.py', 'test/test_{basename}.py', 'tests/test_{basename}.py'],
      \   },
      \   'test/*.py': {'type': 'test'},
      \   'tests/*.py': {'type': 'test'},
      \ }})
