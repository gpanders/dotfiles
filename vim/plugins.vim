" Plugins {{{
" Specify a directory for plugins
call plug#begin('~/.vim/plug')

Plug 'tpope/vim-sensible'       " Sensible defaults (most already incorporated into neovim)
Plug 'tpope/vim-unimpaired'     " A bunch of useful keybindings
Plug 'tpope/vim-capslock'       " Software caps lock
Plug 'tpope/vim-fugitive'       " Useful git commands
Plug 'tpope/vim-commentary'     " Keybindings for commenting
Plug 'tpope/vim-surround'       " Manage parenthises and brackets
Plug 'tpope/vim-repeat'         " Extend . to work with plugins

" Automatically save and manage sessions
Plug 'tpope/vim-obsession'
Plug 'dhruvasagar/vim-prosession'

" Solarized color scheme
" Plug 'altercation/vim-colors-solarized'
Plug 'lifepillar/vim-solarized8'

" Use pairs of characters as a motion command
Plug 'justinmk/vim-sneak'

" Show git status icons in gutter
Plug 'airblade/vim-gitgutter'

" Use navigation keybindings in command mode
Plug 'vim-utils/vim-husk'

" Use same bindings to navigate between vim splits and tmux panes
" Plug 'christoomey/vim-tmux-navigator'

" Easy motion
" Plug 'easymotion/vim-easymotion'

" Show marks in gutter
" Plug 'kshenoy/vim-signature'

" Lightline (more lightweight version of vim-airline)
Plug 'itchyny/lightline.vim'
" Plug 'taohexxx/lightline-buffer'
Plug 'mgee/lightline-bufferline'

" Fix vim fold updating for better performance
" Plug 'Konfekt/FastFold'

" FZF
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" Asynchronous maker/linter
Plug 'neomake/neomake'

" Language specific plugins {{{
" C++
Plug 'octol/vim-cpp-enhanced-highlight', { 'for': ['c', 'cpp'] }
Plug 'Shougo/neoinclude.vim', { 'for': ['c', 'cpp'] }

" Java
Plug 'artur-shaik/vim-javacomplete2', { 'for': 'java' }

" Python
Plug 'davidhalter/jedi', { 'for': 'python' }

" Vimscript
Plug 'Shougo/neco-vim', { 'for': 'vim' }
" }}}

if has('nvim')
  " Neovim specific plugins
  " Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  " Plug 'zchee/deoplete-clang', { 'for': ['c', 'cpp'] }
  " Plug 'zchee/deoplete-jedi', { 'for': 'python' }
else
  " Vim specific plugins
  " Plug 'justmao945/vim-clang', { 'for': ['c', 'cpp'] }
endif

" Initialize plugin system
call plug#end()
" }}}

" vim: nofoldenable
