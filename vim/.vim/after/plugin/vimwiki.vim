" vimwiki configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-12

if !get(g:, 'loaded_vimwiki', 0)
  finish
endif

let g:vimwiki_hl_headers = 1
let g:vimwiki_folding = 'expr'
let g:vimwiki_dir_link = 'index'

" Extend wikis with default values
call map(g:vimwiki_list, { _, val -> extend(val,
      \ {
      \   'path_html': simplify(val.path . '/html'),
      \   'template_path': $MYVIMRUNTIME . '/plugin/vimwiki/templates/',
      \   'css_name': $MYVIMRUNTIME . '/plugin/vimwiki/style.css',
      \   'auto_tags': 1,
      \   'auto_toc': 1,
      \ }, 'keep')})
