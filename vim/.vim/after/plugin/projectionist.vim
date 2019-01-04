if !exists('g:loaded_projectionist')
  finish
endif

let g:projectionist_heuristics ={
      \ "Cargo.toml&src/": {
      \   "src/*.rs": {"type": "src"},
      \   "Cargo.toml": {"type": "toml" }
      \ }}
