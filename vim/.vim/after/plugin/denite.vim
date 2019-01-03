if !exists('g:loaded_denite')
  finish
endif

if executable('rg')
  call denite#custom#var('file/rec', 'command',
        \ ['rg', '--files', '--glob', '!.git']) 
  call denite#custom#var('grep', 'command', ['rg'])
  call denite#custom#var('grep', 'default_opts',
        \ ['-S', '--vimgrep', '--no-heading'])
  call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
  call denite#custom#var('grep', 'recursive_opts', [])
  call denite#custom#var('grep', 'separator', ['--'])
  call denite#custom#var('grep', 'final_opts', [])
elseif executable('ag')
  call denite#custom#var('file/rec', 'command',
        \ ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
  call denite#custom#var('grep', 'command', ['rg'])
  call denite#custom#var('grep', 'default_opts',
        \ ['-S', '--vimgrep'])
  call denite#custom#var('grep', 'pattern_opt', [])
  call denite#custom#var('grep', 'recursive_opts', [])
  call denite#custom#var('grep', 'separator', ['--'])
  call denite#custom#var('grep', 'final_opts', [])
endif

call denite#custom#map(
      \ 'insert',
      \ '<C-n>',
      \ '<denite:move_to_next_line>',
      \ 'noremap'
      \)
call denite#custom#map(
      \ 'insert',
      \ '<C-p>',
      \ '<denite:move_to_previous_line>',
      \ 'noremap'
      \)

call denite#custom#map('insert', '<C-b>', '<denite:move_caret_to_left>', 'noremap')
call denite#custom#map('insert', '<C-f>', '<denite:move_caret_to_right>', 'noremap')
call denite#custom#map('insert', '<C-d>', '<denite:delete_char_under_caret>', 'noremap')

" Mappings
nnoremap <leader>d :Denite <C-D><C-Z>
nnoremap <silent> <C-P> :Denite file/rec<CR>
nnoremap <leader>T :Denite tag<CR>
nnoremap <leader>B :Denite buffer<CR>
nnoremap <leader>y :Denite register<CR>
nnoremap <leader>k :Denite mark<CR>
