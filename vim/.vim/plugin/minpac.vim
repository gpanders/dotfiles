let s:save_cpo = &cpo
set cpo&vim

function! s:PackInit()
  silent! packadd minpac

  if exists('*minpac#init')
    call minpac#init()
    call minpac#add('k-takata/minpac', {'type': 'opt'})

    call minpac#add('gpanders/vim-man', {'type': 'opt'})
    call minpac#add('gpanders/vim-oldfiles')

    " Tim Pope plugin suite
    call minpac#add('tpope/vim-fugitive')
    call minpac#add('tpope/vim-surround')
    call minpac#add('tpope/vim-repeat')
    call minpac#add('tpope/vim-commentary')
    call minpac#add('tpope/vim-dispatch')
    call minpac#add('tpope/vim-projectionist')
    call minpac#add('tpope/vim-rsi')
    call minpac#add('tpope/vim-eunuch')
    call minpac#add('tpope/vim-characterize')
    call minpac#add('tpope/vim-abolish')
    call minpac#add('tpope/vim-speeddating')
    call minpac#add('tpope/vim-scriptease')

    " Better directory browser
    call minpac#add('justinmk/vim-dirvish')

    " More, better, and up-to-date language packs (ftplugins, syntax files, etc)
    call minpac#add('sheerun/vim-polyglot')

    " Align lines to a character, e.g. =, ;, :, etc.
    call minpac#add('junegunn/vim-easy-align')

    " Distraction-free writing
    call minpac#add('junegunn/goyo.vim')

    " Show change signs in the gutter for git files
    call minpac#add('mhinz/vim-signify')

    " Auto generate tags files
    call minpac#add('ludovicchabant/vim-gutentags')

    " Note taking and knowledge tracking
    call minpac#add('vimwiki/vimwiki', {'rev': 'dev'})

    " Populate results of :ilist and :dlist in quickfix window
    call minpac#add('romainl/vim-qlist')

    " Language specific
    " Python
    call minpac#add('drgarcia1986/python-compilers.vim')

    " LaTeX
    call minpac#add('lervag/vimtex')

    " Language Server Client
    " call minpac#add('neoclide/coc.nvim', {'type': 'opt', 'rev': 'release'})
    " Asynchronous linting
    call minpac#add('w0rp/ale', {'type': 'opt'})

    " Colorschemes
    call minpac#add('chriskempson/base16-vim', {'type': 'opt'})
  endif
endfunction

let s:file = expand('<sfile>:p')
command! PackUpdate execute 'source ' . s:file |
      \ call <SID>PackInit() |
      \ try |
      \   call minpac#update('', {'do': 'call minpac#status()'}) |
      \ catch |
      \ endtry

command! PackClean  execute 'source ' . s:file |
      \ call <SID>PackInit() |
      \ try |
      \   call minpac#clean() |
      \ catch |
      \ endtry

let &cpo = s:save_cpo
unlet s:save_cpo
