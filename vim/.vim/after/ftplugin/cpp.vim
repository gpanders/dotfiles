" C / C++
setlocal shiftwidth=2
setlocal tabstop=2
setlocal softtabstop=2
setlocal commentstring=//%s
setlocal cindent

if executable('clang-format')
  setlocal formatprg=clang-format
endif
