" Rust
if &filetype !=# 'rust'
  finish
endif

if executable('rustfmt')
  setl formatprg=rustfmt

  if exists('b:undo_ftplugin')
    let b:undo_ftplugin .= '|setl fp<'
  else
    let b:undo_ftplugin = 'setl fp<'
  endif
endif

let g:rustfmt_emit_file = 1
let b:rustfmt_autosave = 1
