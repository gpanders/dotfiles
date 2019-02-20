" Rust
if &filetype !=# 'rust'
  finish
endif

if executable('rustfmt')
  setl equalprg=rustfmt

  if exists('b:undo_ftplugin')
    let b:undo_ftplugin .= '|setl ep<'
  else
    let b:undo_ftplugin = 'setl ep<'
  endif
endif

let g:rustfmt_emit_file = 1
let b:rustfmt_autosave = 1
