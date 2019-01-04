" Rust
if &filetype !=# 'rust'
  finish
endif

setl equalprg=rustfmt

let b:rustfmt_autosave = 1
