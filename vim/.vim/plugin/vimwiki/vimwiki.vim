" vimwiki configuration
" This file is executed BEFORE vimwiki is loaded
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-12

" Machine specific wiki paths
runtime wikis.vim

let g:vimwiki_hl_headers = 1
let g:vimwiki_dir_link = 'index'

" Extend wikis with default values
if exists('g:vimwiki_list')
  call map(g:vimwiki_list, { _, val -> extend(val,
        \ {
        \   'path_html': simplify(val.path . '/html'),
        \   'template_path': $MYVIMRUNTIME . '/plugin/vimwiki/templates/',
        \   'css_name': $MYVIMRUNTIME . '/plugin/vimwiki/style.css',
        \   'auto_tags': 1,
        \   'auto_toc': 1,
        \ }, 'keep')})
endif
