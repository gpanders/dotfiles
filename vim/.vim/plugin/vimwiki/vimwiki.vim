" vimwiki configuration
" This file is executed BEFORE vimwiki is loaded
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-12

" Machine specific wiki paths
" This file should be located at ~/.vim/wikis.vim (or ~/vimfiles/wikis.vim on
" Windows)
runtime wikis.vim

let g:vimwiki_global_ext = 0
let g:vimwiki_hl_headers = 1
let g:vimwiki_dir_link = 'index'

" Extend wikis with default values
if !empty(get(g:, 'vimwiki_list', []))
  function! s:extend(index, wiki)
    if get(a:wiki, 'syntax', '') ==# 'markdown'
      " Settings for markdown wikis
      call extend(a:wiki, {
            \ 'ext': '.md',
            \ 'custom_wiki2html': $VIMHOME . '/plugin/vimwiki/convert.py',
            \ 'custom_wiki2html_args':
            \   get(g:, 'vimwiki_html_header_numbering') ? '-N' : '',
            \ 'template_default': 'pandoc',
            \ 'list_margin': 0,
            \ }, 'keep')
    else
      " Settings for wiki-style wikis
      call extend(a:wiki, {
            \ 'auto_toc': 1,
            \ 'template_default': 'default',
            \ }, 'keep')
    endif

    " Settings for all wikis
    call extend(a:wiki, {
          \ 'path_html': simplify(a:wiki.path . '/html'),
          \ 'template_path': $VIMHOME . '/plugin/vimwiki/templates/',
          \ 'template_ext': 'html',
          \ 'auto_tags': 1,
          \ }, 'keep')

    return a:wiki
  endfunction

  call map(filter(g:vimwiki_list, '!empty(v:val.path)'), function('s:extend'))
endif
