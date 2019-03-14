" vim-startify configuration
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-02-13

if !get(g:, 'loaded_startify', 0)
  finish
endif

let g:startify_session_persistence = 1
let g:startify_change_to_vcs_root = 1
let g:startify_fortune_use_unicode = 1
let g:startify_session_dir = $MYVIMRUNTIME . '/session'
let g:startify_bookmarks = [{'c': $MYVIMRC}]

" Add wikis to bookmarks
if exists('g:vimwiki_list')
  if len(g:vimwiki_list) > 1
    let cnt = 1
  else
    let cnt = ''
  endif

  for wiki in g:vimwiki_list
    let wiki_keys = keys(wiki)
    if index(wiki_keys, 'index') == -1
      let wiki_index = 'index'
    else
      let wiki_index = wiki.index
    endif
    if index(wiki_keys, 'ext') == -1
      let wiki_ext = '.wiki'
    else
      let wiki_ext = wiki.ext
    endif
    let wiki_key = 'w' . cnt
    let diary_key = 'd' . cnt
    let wiki_path = simplify(wiki.path . '/' . wiki_index . wiki_ext)
    let diary_path = simplify(wiki.path . '/diary/diary' . wiki_ext)
    let cnt += 1
    call extend(g:startify_bookmarks,
          \ [{wiki_key: wiki_path}, {diary_key: diary_path}])
  endfor
endif

" Use ~ for $HOME in Startify bookmarks
for bookmark in g:startify_bookmarks
    let [key, value] = items(bookmark)[0]
    let bookmark[key] = substitute(value, $HOME, '~', '')
endfor
