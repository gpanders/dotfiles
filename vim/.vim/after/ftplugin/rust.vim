" Rust
if &filetype !=# 'rust'
  finish
endif

setl equalprg=rustfmt

let g:rustfmt_emit_file = 1
let b:rustfmt_autosave = 1
