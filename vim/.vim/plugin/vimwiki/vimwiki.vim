" vimwiki configuration
" This file is executed BEFORE vimwiki is loaded
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-12

" Machine specific wiki paths
" This file should be located at $MYVIMRUNTIME/wikis.vim
runtime wikis.vim

let g:vimwiki_hl_headers = 1
let g:vimwiki_dir_link = 'index'
let g:vimwiki_toc_header_level = 1

" Extend wikis with default values
if exists('g:vimwiki_list')
  call map(g:vimwiki_list, { _, val -> extend(val,
        \ {
        \   'syntax': 'markdown',
        \   'ext': '.md',
        \   'path_html': simplify(val.path . '/html'),
        \   'custom_wiki2html': $MYVIMRUNTIME . '/plugin/vimwiki/convert.py',
        \   'template_path': $MYVIMRUNTIME . '/plugin/vimwiki/templates/',
        \   'template_default': 'mindoc-pandoc',
        \   'template_ext': 'html',
        \   'css_name': $MYVIMRUNTIME . '/plugin/vimwiki/style.css',
        \   'auto_tags': 1,
        \ }, 'keep')})
endif
