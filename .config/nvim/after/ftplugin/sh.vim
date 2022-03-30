if executable('shfmt')
  if get(b:, 'is_sh')
    let &l:formatprg = 'shfmt -s -p -'
  else
    let &l:formatprg = 'shfmt -s -'
  endif
endif

let b:undo_ftplugin .= '|setl fp<'

compiler shellcheck
