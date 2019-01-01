function! s:PackInit()
  packadd minpac

  if exists('*minpac#init')
    call minpac#init()
    call minpac#add('k-takata/minpac', {'type': 'opt'})

    call minpac#add('tpope/vim-unimpaired')
    call minpac#add('tpope/vim-fugitive')
    call minpac#add('tpope/vim-surround')
    call minpac#add('tpope/vim-repeat')
    call minpac#add('tpope/vim-commentary')
    call minpac#add('tpope/vim-rhubarb')
    call minpac#add('tpope/vim-dispatch')
    call minpac#add('tpope/vim-rsi')
    call minpac#add('justinmk/vim-dirvish')
    call minpac#add('justinmk/vim-sneak')
    call minpac#add('junegunn/fzf.vim')
    call minpac#add('junegunn/limelight.vim')
    call minpac#add('junegunn/goyo.vim')
    call minpac#add('airblade/vim-gitgutter')
    call minpac#add('ludovicchabant/vim-gutentags')
    call minpac#add('mhinz/vim-startify')
    call minpac#add('jpalardy/vim-slime')
    call minpac#add('vimwiki/vimwiki')
    " call minpac#add('davidhalter/jedi-vim')
    call minpac#add('romainl/vim-qf')
    call minpac#add('lervag/vimtex')

    " Neovim or Vim 8.1+ required
    if has('nvim')
      call minpac#add('neoclide/coc.nvim', {'do': 'call coc#util#install()'})
    elseif v:version >= 801
      call minpac#add('neoclide/coc.nvim', {'do': 'call coc#util#install() | silent! !yarn global add vim-node-rpc'})
    endif

    " Colorschemes
    call minpac#add('joshdick/onedark.vim', {'type': 'opt'})
    call minpac#add('lifepillar/vim-solarized8', {'type': 'opt'})
  endif
endfunction

command! PackUpdate  call <SID>PackInit() | call minpac#update('', {'do': 'call minpac#status()'})
command! PackClean   call <SID>PackInit() | call minpac#clean()
command! PackStatus  call <SID>PackInit() | call minpac#status()
