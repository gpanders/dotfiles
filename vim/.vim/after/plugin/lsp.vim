" vim-lsp configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2018-12-03

if !get(g:, 'lsp_loaded', 0)
  finish
endif

" cquery
if executable('cquery')
   au User lsp_setup call lsp#register_server({
      \ 'name': 'cquery',
      \ 'cmd': {server_info->['cquery']},
      \ 'root_uri': {server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'compile_commands.json'))},
      \ 'initialization_options': { 'cacheDirectory': '/tmp/cquery/cache' },
      \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp', 'cc'],
      \ })

   au FileType c,cpp noremap <buffer> gd :LspDefinition<CR>
   au FileType c,cpp setlocal completeopt+=preview
endif

" pyls
" if executable('pyls')
"   au User lsp_setup call lsp#register_server({
"         \ 'name': 'pyls',
"         \ 'cmd': {server_info->['pyls']},
"         \ 'whitelist': ['python'],
"         \ })

"   au FileType python nnoremap <buffer> gd :LspDefinition<CR>
"   au FileType python setlocal completeopt+=preview
" endif
