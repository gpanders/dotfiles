" Projections for cargo projects
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-20

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

let g:projectionist_heuristics = get(g:, 'projectionist_heuristics', {})

call extend(g:projectionist_heuristics, {
      \ 'Cargo.toml&src/': {
      \   'src/*.rs': {'type': 'src'},
      \   'Cargo.toml': {'type': 'toml' }
      \ }})
