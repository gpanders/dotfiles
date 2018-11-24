if executable('rg')
  let g:ackprg = 'rg --vimgrep'
elseif executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

let g:ack_use_dispatch = 0

noremap <C-K> :Ack!<space>
