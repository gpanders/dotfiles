if exists('plugs') && has_key(plugs, 'coc.nvim')
  noremap <silent> gd <Plug>(coc-definition)
  inoremap <silent><expr> <C-Space> coc#refresh()
  noremap <silent> K :call <SID>show_documentation()<CR>

  function! s:show_documentation()
    if &filetype == 'vim'
      execute 'h '.expand('<cword>')
    else
      call CocAction('doHover')
    endif
  endfunction

  augroup CocSetup
    au!
    " Highlight symbol under cursor on CursorHold
    autocmd CursorHold * silent call CocActionAsync('highlight')
    
    autocmd FileType c,cpp,python setlocal formatexpr=CocAction('formatSelected')
  augroup END
endif
