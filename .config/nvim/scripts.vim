" Vim support file to detect file types in scripts

if did_filetype()
  finish
endif

let s:line1 = getline(1)

if s:line1 =~# '-*- C++ -*-'
  setfiletype cpp
endif
