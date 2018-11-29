" C / C++
setlocal sw=2
setlocal ts=2
setlocal sts=2
setlocal cms=//%s
setlocal cin

if executable('clang-format')
  setlocal formatprg=clang-format
endif
