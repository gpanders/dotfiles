function! s:PackInit()
  packadd minpac

  if exists('*minpac#init')
    call minpac#init()

    call minpac#add('gpanders/vim-man')
    call minpac#add('gpanders/vim-oldfiles')

    " Tim Pope plugin suite
    call minpac#add('tpope/vim-unimpaired')
    call minpac#add('tpope/vim-fugitive')
    call minpac#add('tpope/vim-surround')
    call minpac#add('tpope/vim-repeat')
    call minpac#add('tpope/vim-commentary')
    call minpac#add('tpope/vim-rhubarb')
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

    " Like f or t but uses 2 characters, e.g. ssn to jump to 'sneak'
    call minpac#add('justinmk/vim-sneak')

    " More, better, and up-to-date language packs (ftplugins, syntax files, etc)
    call minpac#add('sheerun/vim-polyglot')

    " Align lines to a character, e.g. =, ;, :, etc.
    call minpac#add('junegunn/vim-easy-align')

    " Fuzzy file finder and bonus commands
    " call minpac#add('junegunn/fzf.vim')

    " Fun plugin for writing plain text. Highlight current paragraph
    call minpac#add('junegunn/limelight.vim')

    " Use :Goyo to enable focus mode
    call minpac#add('junegunn/goyo.vim')

    " Show change signs in the gutter for git files
    call minpac#add('airblade/vim-gitgutter')

    " Auto generate tags files
    call minpac#add('ludovicchabant/vim-gutentags')

    " Start page for vim
    call minpac#add('mhinz/vim-startify')

    " Make working with REPLs so much better
    call minpac#add('jpalardy/vim-slime')

    " Note taking and knowledge tracking
    call minpac#add('vimwiki/vimwiki')

    " Quickfix window improvements
    call minpac#add('romainl/vim-qf')

    " Populate results of :ilist and :dlist in quickfix window
    call minpac#add('romainl/vim-qlist')

    " Run tests from within vim
    call minpac#add('janko/vim-test')

    " Add more text objects to operate on
    call minpac#add('wellle/targets.vim')

    " Code snippets
    call minpac#add('SirVer/ultisnips')

    " Language specific
    " Python
    call minpac#add('davidhalter/jedi-vim')
    call minpac#add('drgarcia1986/python-compilers.vim')

    " LaTeX
    call minpac#add('lervag/vimtex')

    " Neovim or Vim 8.1+ required
    if has('nvim') || v:version >= 801
      " Language Server Client
      " call minpac#add('neoclide/coc.nvim', {'do': 'call coc#util#install()'})
      " Asynchronous linting
      call minpac#add('w0rp/ale')
    endif

    " Colorschemes
    call minpac#add('romainl/flattened')
    call minpac#add('romainl/Apprentice')
    call minpac#add('chriskempson/base16-vim')
  endif
endfunction

command! PackUpdate call <SID>PackInit() | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean  call <SID>PackInit() | call minpac#clean()
command! PackStatus call <SID>PackInit() | call minpac#status()
