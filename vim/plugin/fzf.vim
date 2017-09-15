let g:fzf_tags_command = 'ctags -R'

map <silent> <C-P> :Files<CR>
map <silent> <C-T> :Tags<CR>
map <silent> <C-Y> :BTags<CR>
map <silent> <C-B> :Buffers<CR>

" Augmenting Ag command using fzf#vim#with_preview function
"   * fzf#vim#with_preview([[options], preview window, [toggle keys...]])
"     * For syntax-highlighting, Ruby and any of the following tools are required:
"       - Highlight: http://www.andre-simon.de/doku/highlight/en/highlight.php
"       - CodeRay: http://coderay.rubychan.de/
"       - Rouge: https://github.com/jneen/rouge
"
"   :Ag  - Start fzf with hidden preview window that can be enabled with "?" key
"   :Ag! - Start fzf in fullscreen and display the preview window above
command! -bang -nargs=* Ag
      \ call fzf#vim#ag(<q-args>,
      \                 <bang>0 ? fzf#vim#with_preview('up:60%')
      \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
      \                 <bang>0)

command! -bang -nargs=* Tags
      \ call fzf#vim#tags(<q-args>,
      \                   <bang>0 ? fzf#vim#with_preview('up:60%')
      \                           : fzf#vim#with_preview('right:50%:hidden', '?'),
      \                   <bang>0)

nnoremap \\ :Ag<space>
nnoremap K :Ag <C-R><C-W><CR>

function! s:fzf_statusline()
  set laststatus=0
  autocmd BufWinLeave <buffer> set laststatus=2
endfunction

autocmd! User FzfStatusLine call <SID>fzf_statusline()
