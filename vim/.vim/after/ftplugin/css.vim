" css filetype plugin
" Author: Greg Anders <greg@gpanders.com>

if &filetype !=# 'css'
  finish
endif

let b:undo_ftplugin = get(b:, 'undo_ftplugin', '')

let g:css_format_on_write = 1

if executable('prettier')
  setlocal formatprg=prettier\ %
endif

if !empty(&l:formatprg)
  let b:undo_ftplugin .= '|setl fp<'
  augroup ftplugin.css
    autocmd!
    autocmd BufWritePre <buffer>
          \ if g:css_format_on_write |
          \   let view = winsaveview() |
          \   execute '%!' . &l:formatprg |
          \   call winrestview(view) |
          \   unlet view |
          \ endif
  augroup END
  let b:undo_ftplugin .= '|exe "au! ftplugin.css * <buffer>"'
endif
