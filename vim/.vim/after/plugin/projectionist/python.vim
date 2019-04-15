" Projections for projects with compile_commands.json
" Author: Greg Anders <greg@gpanders.com>
" Date: 2019-03-20

if !get(g:, 'loaded_projectionist', 0)
  finish
endif

function! s:detect() abort
  let root = g:projectionist_file
  let previous = ''
  while root !=# previous && root != '.'
    if ProjectionistHas('setup.py', root)
      call projectionist#append(root, {'setup.py': {'type': 'setup'}})
      let pkgname = fnamemodify(root, ':t')
      let testdir = matchstr(glob('test*'), '^tests\?$')
      if !empty(testdir)
        call projectionist#append(root, {
              \ '*.py': { 'path': testdir . '/**' },
              \ testdir . '/**/test_*.py': {
              \   'type': 'test',
              \ }})
      endif

      let srcdir = matchstr(glob('**/' . pkgname, 0, 1), '^\%(src/\)\?' . pkgname . '$')
      if !empty(srcdir)
        call projectionist#append(root, {
              \ '*.py': { 'path': srcdir . '/**' },
              \ srcdir . '/*.py': {
              \   'type': 'src',
              \   'alternate': [
              \     'test/test_{basename}.py',
              \     'tests/test_{basename}.py',
              \     '{dirname}/test_{basename}.py'
              \   ],
              \ }})
      endif
      break
    endif
    let previous = root
    let root = fnamemodify(root, ':h')
  endwhile
endfunction

autocmd User ProjectionistDetect call s:detect()
