" Plugins {{{
" Specify a directory for plugins
call plug#begin('~/.vim/plug')

Plug 'tpope/vim-sensible'        " Sensible defaults
Plug 'tpope/vim-unimpaired'      " A bunch of useful keybindings
Plug 'tpope/vim-capslock'        " Software caps lock
Plug 'tpope/vim-commentary'      " Keybindings for commenting
Plug 'tpope/vim-surround'        " Manage parenthises and brackets
Plug 'tpope/vim-repeat'          " Extend . to work with plugins
Plug 'tpope/vim-vinegar'         " Better netrw / file browser
Plug 'tpope/vim-dispatch'        " Asynchronous task runner
Plug 'tpope/vim-fugitive'        " Vim git wrapper
Plug 'tpope/vim-rhubarb'         " Github plugin for fugitive

" Gitlab plugin for fugitive
Plug 'shumphrey/fugitive-gitlab.vim'

" Automatically save and manage sessions
Plug 'tpope/vim-obsession'
Plug 'dhruvasagar/vim-prosession'

" Solarized color scheme
Plug 'lifepillar/vim-solarized8'

" One Dark color scheme
Plug 'joshdick/onedark.vim'

" Retro groove color scheme for Vim
Plug 'morhetz/gruvbox'

" A solid language pack for Vim
Plug 'sheerun/vim-polyglot'

" Improved grepping with support for ag and ripgrep
Plug 'mileszs/ack.vim'

" Use pairs of characters as a motion command
Plug 'justinmk/vim-sneak'

" Show git status icons in gutter
Plug 'airblade/vim-gitgutter'

" Use navigation keybindings in command mode
Plug 'vim-utils/vim-husk'

" A Vim plugin that manages your tag files
Plug 'ludovicchabant/vim-gutentags'

" Use same bindings to navigate between vim splits and tmux panes
" Plug 'christoomey/vim-tmux-navigator'

" FZF
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" Vim language server client
" Plug 'neoclide/coc.nvim', { 'tag': '*', 'do': { -> coc#util#install() }}
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete-lsp.vim'

" Language specific plugins {{{
" C++
Plug 'Shougo/neoinclude.vim', { 'for': ['c', 'cpp'] }

" Python
Plug 'davidhalter/jedi-vim', { 'for': 'python' }

" Vimscript
Plug 'Shougo/neco-vim', { 'for': 'vim' }
" }}}

" Initialize plugin system
call plug#end()
" }}}

" vim: nofoldenable
