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

let s:async = (has('nvim') || has('job')) && executable('jq')

function! s:parse_compile_commands(root)
  if s:async
    call async#run([
          \ 'jq',
          \ '-c',
          \ '[[.[].command?, .[].arguments[]?] | join(" ") | match("-(?:I|isystem )(\\S+)"; "g") | .captures[0].string] | unique',
          \ a:root . '/compile_commands.json'
          \ ], {paths -> s:set_path(a:root, eval(get(paths, 0, '[]')))})
  else
    let compile_commands = projectionist#json_parse(readfile(a:root . '/compile_commands.json'))
    let cmds = []
    for item in compile_commands
      if has_key(item, 'arguments')
        let cmd = join(item.arguments)
      elseif has_key(item, 'command')
        let cmd = item.command
      else
        continue
      endif
      let cmds += [cmd]
    endfor
    " Add every match of -I<dir> or -isystem <dir> to paths
    let paths = []
    call substitute(join(cmds),
          \ '\C\-\%(I\|isystem \)\(\f\+\)', '\=add(paths, submatch(1))', 'g')

    let paths = uniq(sort(paths))
    call s:set_path(a:root, paths)
  endif
endfunction

function! s:set_path(root, ...)
  if a:0
    let paths = a:1
    let s:paths[a:root] = filter(map(paths, "fnamemodify(v:val, ':p')"), 'isdirectory(v:val)')
  endif
  for dir in s:paths[a:root]
    if stridx(',' . &l:path . ',', ',' . escape(dir, ', ') . ',') < 0
      let &l:path = escape(dir, ', ') . ',' . &path
    endif
  endfor
endfunction

function! s:activate() abort
  let root = projectionist#path()
  if ProjectionistHas('compile_commands.json', root)
    let s:paths = {}
    if !has_key(s:paths, root)
      call s:parse_compile_commands(root)
    else
      call s:set_path(root)
    endif
  endif
endfunction

autocmd User ProjectionistActivate
      \ if &ft ==# 'c' || &ft ==# 'cpp' |
      \   call s:activate() |
      \ endif
