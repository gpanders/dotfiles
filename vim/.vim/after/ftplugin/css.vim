" css filetype plugin
" Author: Greg Anders <greg@gpanders.com>

if &filetype !=# 'css'
  finish
endif

let g:css_format_on_write = 1

if executable('prettier')
  setlocal formatprg=prettier\ %
  let b:undo_ftplugin = get(b:, 'undo_ftplugin', '') . '|setl fp<'
  augroup css.vim.PreWrite
    autocmd!
    autocmd BufWritePre <buffer>
          \ if g:css_format_on_write |
          \   let view = winsaveview() |
          \   execute '%!' . &l:formatprg |
          \   call winrestview(view) |
          \   unlet view |
          \ endif
  augroup END
  let b:undo_ftplugin .= '|au! css.vim.PreWrite'
endif
