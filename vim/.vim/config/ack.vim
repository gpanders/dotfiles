if executable('rg')
  let g:ackprg = 'rg -S --vimgrep'
elseif executable('ag')
  let g:ackprg = 'ag -S --vimgrep'
endif

let g:ack_use_dispatch = 0

noremap <C-K> :Ack!<space>
