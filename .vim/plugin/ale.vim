" This variable must be set before ALE is loaded so that it can create the
" proper autocmds
let g:ale_lint_on_text_changed = 'normal'

" Don't change syntax highlighting, just use the sign column
let g:ale_set_highlights = 0

let g:ale_disable_lsp = 1
