" Projections for projects with compile_commands.json
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-20

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

let s:paths = {}

function! s:parse_compile_commands(root)
  let compile_commands = join(readfile(a:root . '/compile_commands.json'))
  " Add every match of -I<dir> or -isystem <dir> to paths
  let paths = []
  call substitute(compile_commands,
        \ '\C\-\%(I\|isystem \)\(\f\+\)', '\=add(paths, submatch(1))', 'g')
  let s:paths[a:root] = filter(uniq(paths), 'isdirectory(v:val)')
endfunction

function! s:detect() abort
  let root = g:projectionist_file
  let previous = ''
  while root !=# previous && root !=# '.'
    if ProjectionistHas('compile_commands.json', root)
      if !has_key(s:paths, root)
        call s:parse_compile_commands(root)
      endif
      for dir in s:paths[root]
        if stridx(',' . &l:path . ',', ',' . escape(dir, ', ') . ',') < 0
          let &l:path = &path . ',' . escape(dir, ', ')
        endif
      endfor
      break
    endif
    let previous = root
    let root = fnamemodify(root, ':h')
  endwhile
endfunction

autocmd User ProjectionistDetect
      \ if &ft ==# 'c' || &ft ==# 'cpp' |
      \   call s:detect() |
      \ endif
