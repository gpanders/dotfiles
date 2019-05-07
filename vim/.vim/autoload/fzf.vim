" Use FZF to find help tags
" Author: Greg Anders
" Date: 2019-05-07

function! fzf#helptags(bang)
  if !exists('*fzf#run')
    echohl ErrorMsg
    echom 'FZF installation not found'
    echohl None
    return
  endif

  let tags = uniq(sort(globpath(&rtp, 'doc/tags', 0, 1)))
  call fzf#run(fzf#wrap('Helptags', {
        \ 'source': 'grep -ho "^\S\+" ' . join(tags),
        \ 'sink': 'help',
        \ 'options': '+m'
        \ }, a:bang))
endfunction
