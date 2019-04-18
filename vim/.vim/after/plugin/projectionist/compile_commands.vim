" Projections for projects with compile_commands.json
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-20

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

let g:projectionist_heuristics = get(g:, 'projectionist_heuristics', {})

call extend(g:projectionist_heuristics, {
      \ 'compile_commands.json': {
      \   'src/*.cpp': {
      \     'type': 'src',
      \     'alternate': ['src/{}.h', 'include/{}.h'],
      \   },
      \   'src/*.cc': {
      \     'type': 'src',
      \     'alternate': ['src/{}.h', 'include/{}.h'],
      \   },
      \   'src/*.h': {
      \     'type': 'include',
      \     'alternate': ['src/{}.cpp', 'src/{}.cc'],
      \   },
      \   'include/*.h': {
      \     'type': 'include',
      \     'alternate': ['src/{}.cpp', 'src/{}.cc'],
      \   }
      \ }})

let s:paths = {}

function! s:parse_compile_commands(root)
  let compile_commands = join(readfile(a:root . '/compile_commands.json'))
  " Add every match of -I<dir> or -isystem <dir> to paths
  let paths = []
  call substitute(compile_commands,
        \ '\C\-\%(I\|isystem \)\(\f\+\)', '\=add(paths, submatch(1))', 'g')
  let s:paths[a:root] = filter(uniq(sort(paths)), 'isdirectory(v:val)')
endfunction

function! s:activate() abort
  let root = projectionist#path()
  if ProjectionistHas('compile_commands.json', root)
    if !has_key(s:paths, root)
      call s:parse_compile_commands(root)
    endif
    for dir in s:paths[root]
      if stridx(',' . &l:path . ',', ',' . escape(dir, ', ') . ',') < 0
        if dir[0] ==# '/'
          let &l:path = &path . ',' . escape(dir, ', ')
        else
          let &l:path = escape(dir, ', ') . ',' . &path
        endif
      endif
    endfor
  endif
endfunction

autocmd User ProjectionistActivate
      \ if &ft ==# 'c' || &ft ==# 'cpp' |
      \   call s:activate() |
      \ endif
