if exists("did_load_filetypes")
  finish
endif

augroup filetypedetect
  autocmd! BufRead,BufNewFile *.bd                            setf json
  autocmd! BufRead,BufNewFile *.vho                           setf vhdl
  autocmd! BufRead,BufNewFile ~/.local/share/zsh/functions/*  setf zsh
augroup END
