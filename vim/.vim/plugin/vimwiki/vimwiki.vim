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
  function! s:extend(index, wiki)
    if get(a:wiki, 'syntax', 'markdown') ==# 'markdown'
      " Settings for markdown wikis
      call extend(a:wiki, {
            \ 'syntax': 'markdown',
            \ 'ext': '.md',
            \ 'custom_wiki2html': $MYVIMRUNTIME . '/plugin/vimwiki/convert.py',
            \ 'template_default': 'mindoc-pandoc',
            \ 'template_ext': 'html',
            \ }, 'keep')
    else
      " Settings for wiki-style wikis
      call extend(a:wiki, {
            \ 'auto_toc': 1,
            \ }, 'keep')
    endif

    " Settings for all wikis
    call extend(a:wiki, {
          \ 'path_html': simplify(a:wiki.path . '/html'),
          \ 'template_path': $MYVIMRUNTIME . '/plugin/vimwiki/templates/',
          \ 'css_name': $MYVIMRUNTIME . '/plugin/vimwiki/style.css',
          \ 'auto_tags': 1,
          \ }, 'keep')

    return a:wiki
  endfunction

  call map(filter(g:vimwiki_list, '!empty(v:val.path)'), function('s:extend'))
endif
