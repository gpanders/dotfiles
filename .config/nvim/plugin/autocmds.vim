augroup init
  autocmd!

  " Close preview and command windows with q
  autocmd BufWinEnter * if &previewwindow | nnoremap <buffer> q <C-W>q | endif
  autocmd CmdWinEnter * nnoremap <buffer> q <C-W>q

  " Highlight yanked text
  autocmd TextYankPost * lua vim.highlight.on_yank {higroup="Visual", timeout=150, on_visual=true}

  " Start insert mode in terminals automatically (and set the statusline to
  " the terminal title)
  autocmd TermOpen * setlocal statusline=%{b:term_title} | startinsert

  " Auto close shell terminals (#15440)
  autocmd TermClose *
        \ if !v:event.status |
        \   let info = nvim_get_chan_info(&channel) |
        \   if get(info, 'argv', []) ==# [&shell] |
        \     exec 'bdelete! ' .. expand('<abuf>') |
        \   endif |
        \ endif

  " Hide cursorline in insert mode and when the current window doesn't have
  " focus
  autocmd InsertEnter,WinLeave,FocusLost * setlocal nocursorline
  autocmd InsertLeave,WinEnter,FocusGained * if mode() !=# 'i' | let &l:cursorline = 1 | endif

  " Create missing parent directories automatically
  autocmd BufNewFile * autocmd BufWritePre <buffer> ++once call mkdir(expand('%:h'), 'p')

  " Defer setting the colorscheme until the UI loads (micro optimization)
  autocmd UIEnter * colorscheme base16-eighties

  " Restore cursor position (except for git commits and rebases). This
  " autocommand can't be defined in init.vim since it needs to be created
  " _after_ the FileType autocommands in filetype.vim.
  autocmd BufRead * if &ft !~# 'commit\|rebase' | exec 'silent! normal! g`"' | endif

  " Don't show trailing spaces in insert mode
  autocmd InsertEnter * setlocal listchars-=trail:-
  autocmd InsertLeave * setlocal listchars+=trail:-
augroup END
