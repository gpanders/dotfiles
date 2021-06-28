if exists('did_load_filetypes')
  finish
endif

augroup filetypedetect
  autocmd! BufRead,BufNewFile *.bd                            setf json
  autocmd! BufRead,BufNewFile *.vho                           setf vhdl
  autocmd! BufRead,BufNewFile ~/.local/share/zsh/functions/*  setf zsh
  autocmd! BufRead,BufNewFile *.env                           setf env
  autocmd! BufRead,BufNewFile *.cl                            setf opencl
  autocmd! BufRead,BufNewFile *.fish                          setf fish
  autocmd! BufRead,BufNewFile *.zig                           setf zig
  autocmd! BufRead,BufNewFile *.nix                           setf nix
augroup END
