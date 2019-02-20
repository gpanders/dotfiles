if !exists('g:loaded_ale')
  finish
endif

let g:ale_lint_on_text_changed = 'normal'

let g:ale_fixers = {
      \ 'python': ['isort', 'yapf', 'add_blank_lines_for_python_control_statements'],
      \ '*': ['remove_trailing_lines', 'trim_whitespace']
      \}

nnoremap <Space><C-F> :ALEFix<CR>
