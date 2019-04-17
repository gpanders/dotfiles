" Projections for projects with compile_commands.json
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-20

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

let s:paths = {}

function! s:parse_compile_commands(root)
  try
    let compile_commands = projectionist#json_parse(readfile(a:root . '/compile_commands.json'))
    let s:paths[a:root] = {}
    for item in compile_commands
      " Get file path relative to root
      let file = substitute(item.directory, a:root, '', '') . fnamemodify(item.file, ':r')
      " Add every match of -I<dir> or -isystem <dir> to paths
      let paths = []
      call substitute(join(item.arguments), '\C\-\%(I\|isystem \)\(\f\+\)', '\=add(paths, submatch(1))', 'g')
      let s:paths[a:root][file] = filter(uniq(paths), 'isdirectory(v:val)')
    endfor
  catch /^invalid JSON:/
  endtry
endfunction

function! s:detect() abort
  let root = g:projectionist_file
  let previous = ''
  while root !=# previous && root !=# '.'
    if ProjectionistHas('compile_commands.json', root)
      if !has_key(s:paths, root)
        call s:parse_compile_commands(root)
      endif
      let fname = substitute(expand('%:p:r'), root . '/', '', '')
      if has_key(s:paths[root], fname)
        for dir in s:paths[root][fname]
          if stridx(',' . &l:path . ',', ',' . escape(dir, ', ') . ',') < 0
            let &l:path = &path . ',' . escape(dir, ', ')
          endif
        endfor
      endif
      break
    endif
    let previous = root
    let root = fnamemodify(root, ':h')
  endwhile
endfunction

autocmd User ProjectionistDetect call s:detect()
