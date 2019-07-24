if exists("did_load_filetypes")
  finish
endif

augroup filetypedetect
  autocmd! BufRead,BufNewFile *.bd          setfiletype json
  autocmd! BufRead,BufNewFile *.vho         setfiletype vhdl
  autocmd! BufRead,BufNewFile ~/.zfunc/*    setfiletype zsh
augroup END
