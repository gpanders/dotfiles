if exists('plugs') && has_key(plugs, 'ale')
  let g:ale_fixers = {
        \ '*': ['remove_trailing_lines', 'trim_whitespace'],
        \ 'python': ['autopep8', 'add_blank_lines_for_python_control_statements']
        \}
endif
