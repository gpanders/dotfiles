" Generate include path for python
function! python#include_path()
  let python_include_path = ''
  python3 << EOF
import os
import sys
import vim
for p in sys.path:
  # Add each directory in sys.path if it exists
  if os.path.isdir(p):
    vim.command(r"let python_include_path .= ',%s'" % p)
EOF
  return python_include_path
endfunction
