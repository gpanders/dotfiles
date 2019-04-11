" Projections for projects with compile_commands.json
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-20

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

let g:projectionist_heuristics = get(g:, 'projectionist_heuristics', {})

call extend(g:projectionist_heuristics, {
      \ 'setup.py|requirements.txt': {
      \   '*': { 'python_package': 1 }
      \ }})

function! s:detect() abort
  for [root, value] in projectionist#query('python_package')
    let pkgname = fnamemodify(root, ':t')
    let testdir = matchstr(glob('test*'), '^tests\?$')
    if !empty(testdir)
      call projectionist#append(root, {
            \ '*': { 'path': testdir . '/**' },
            \ testdir . '/**/test_*.py': {
            \   'type': 'test',
            \ }})
    endif

    let srcdir = matchstr(glob('**/' . pkgname, 0, 1), '^\%(src/\)' . pkgname . '$')
    if !empty(srcdir)
      call projectionist#append(root, {
            \ '*': { 'path': srcdir . '/**' },
            \ srcdir . '/*.py': {
            \   'type': 'src',
            \   'alternate': [
            \     'test/test_{basename}.py',
            \     'tests/test_{basename}.py',
            \     '{dirname}/test_{basename}.py'
            \   ],
            \ }})
    endif
  endfor
endfunction

autocmd User ProjectionistDetect call s:detect()
