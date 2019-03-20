" Projections for projects with compile_commands.json
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-20

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

let g:projectionist_heuristics = get(g:, 'projectionist_heuristics', {})

call extend(g:projectionist_heuristics, {
      \ 'compile_commands.json': {
      \   '*': {
      \     'has_compile_commands': 1,
      \   },
      \ }})

function! s:parse_compile_commands(root)
  let compile_commands = join(readfile(expand(a:root . '/compile_commands.json')), '')
  let paths = []
  call substitute(compile_commands, '\C\-\(I\|isystem \)\(\f\+\)', '\=add(paths, submatch(2))', 'g')
  let paths = filter(uniq(paths), 'isdirectory(v:val)')
  for dir in paths
    if stridx(','.&l:path.',', ','.escape(dir, ', ').',') < 0
      let &l:path = escape(dir, ', ') . ',' . &path
    endif
  endfor
endfunction

function! s:activate() abort
  for [root, value] in projectionist#query('has_compile_commands')
    call s:parse_compile_commands(root)
    break
  endfor
endfunction

autocmd User ProjectionistActivate call s:activate()
