" Rust

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

if executable('rustfmt')
  setl formatprg=rustfmt\ -q\ --emit=stdout
  let b:undo_ftplugin .= '|setl fp<'
endif
