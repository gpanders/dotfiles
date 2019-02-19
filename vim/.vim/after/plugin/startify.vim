if !exists('g:loaded_startify')
  finish
endif

let g:startify_session_persistence = 1
let g:startify_bookmarks = [{'c': $MYVIMRC}]

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
    let wiki_path = wiki.path . wiki_index . wiki_ext
    let diary_path = wiki.path . 'diary/diary' . wiki_ext
    let cnt += 1
    call extend(g:startify_bookmarks, [{wiki_key: wiki_path}, {diary_key: diary_path}])
  endfor
endif
