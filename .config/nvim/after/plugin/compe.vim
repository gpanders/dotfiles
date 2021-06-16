if !get(g:, 'loaded_compe')
  finish
endif

let s:source = {'path': v:true, 'nvim_lsp': v:true, 'nvim_lua': v:true}

function! s:setup()
  let buffer = v:true
  for v in ['mail', 'markdown', 'text', 'rst', 'gitcommit', 'gitsendemail']
    if v ==# &filetype
      let buffer = v:false
      break
    endif
  endfor

  call compe#setup({'source': extend(s:source, {'buffer': buffer})}, 0)
endfunction

set completeopt=menuone,noselect
set shortmess+=c

inoremap <silent> <expr> <CR> compe#confirm('<CR>')

augroup my_compe
  autocmd!
  autocmd FileType * call s:setup()
augroup END
