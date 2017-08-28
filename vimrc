" Plugins {{{
" Specify a directory for plugins
call plug#begin(has('nvim') ? '~/.config/nvim/plugins' : '~/.vim/bundle')

Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-capslock'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'dhruvasagar/vim-prosession'
Plug 'altercation/vim-colors-solarized'
Plug 'airblade/vim-gitgutter'
Plug 'vim-utils/vim-husk'
Plug 'christoomey/vim-tmux-navigator'
Plug 'easymotion/vim-easymotion'
Plug 'kshenoy/vim-signature'
Plug 'itchyny/lightline.vim'
Plug 'taohex/lightline-buffer'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'Shougo/neco-syntax'

" Language specific plugins
" C++
Plug 'octol/vim-cpp-enhanced-highlight', { 'for': ['c', 'cpp'] }
Plug 'Shougo/neoinclude.vim', { 'for': ['c', 'cpp'] }

" Java
Plug 'artur-shaik/vim-javacomplete2', { 'for': 'java' }

" Python
Plug 'davidhalter/jedi', { 'for': 'python' }

" Vimscript
Plug 'Shougo/neco-vim', { 'for': 'vim' }

if has('nvim')
    " Neovim specific plugins
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'zchee/deoplete-clang', { 'for': ['c', 'cpp'] }
    Plug 'zchee/deoplete-jedi', { 'for': 'python' }
    Plug 'neomake/neomake'
else
    " Vim specific plugins
    Plug 'tpope/vim-sensible'
    Plug 'Shougo/neocomplete.vim'
    Plug 'Rip-Rip/clang_complete', { 'for': ['c', 'cpp'] }
    Plug 'davidhalter/jedi-vim', { 'for': 'python' }
endif

" Initialize plugin system
call plug#end()
" }}}

" Set the leader key
" This line needs to be before the plugin configuration files so that the
" config files can also use the <leader> value
let mapleader = ','

" Source nvim configs 
if has('nvim')
    runtime! config/**/*.vim
endif

" Set color scheme
set background=light
colorscheme solarized

" Hide buffers instead of closing them
set hidden

" Save by pressing <leader>w
nmap <silent> <leader>w :w<CR>

" Some good default options
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set number
set ignorecase          " Make searching case insensitive
set smartcase           " ... unless the query has capital letters
set gdefault            " Use 'g' flag by default with :s/foo/bar/.
set magic               " Use 'magic' patterns (extended regular expressions)
set hlsearch
set splitbelow          " Open horizontal splits below current pane
set splitright          " Open vertical splits to the right of current pane
set nostartofline       " Don't move cursor to start of line when scrolling

map ; :

" Auto close braces
inoremap {<CR> {<CR>}<Esc>ko

" Show column at 80 characters
set colorcolumn=80

" Set update time
set updatetime=250

" Relative numbering
function! NumberToggle()
  if (&relativenumber == 1)
    set nornu
    set number
  else
    set rnu
  endif
endfunc

" Toggle between normal and relative numbering
nnoremap <leader>r :call NumberToggle()<CR>

" Navigate through wrapped lines individually
nnoremap j gj
nnoremap k gk

" Clear search buffer by pressing <leader><space>
nmap <silent> <leader><space> :nohlsearch<CR>

" Simplify resizing splits
if has('nvim')
  nnoremap <silent> <M-j> :resize +1<CR>
  nnoremap <silent> <M-k> :resize -1<CR>
  nnoremap <silent> <M-h> :vertical resize -1<CR>
  nnoremap <silent> <M-l> :vertical resize +1<CR>
else
  nnoremap <silent> ^[j :resize +1<CR>
  nnoremap <silent> ^[k :resize -1<CR>
  nnoremap <silent> ^[h :vertical resize -1<CR>
  nnoremap <silent> ^[l :vertical resize +1<CR>
endif

" Show substitutions in split window
if has('nvim')
  set inccommand=split
endif

" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

" File-type specific configuration
" C / C++
autocmd FileType c,cpp setlocal sw=2 ts=2 sts=2 cms=//%s

" CMake
autocmd FileType cmake setlocal cms=#%s

" Python
autocmd FileType python setlocal sw=4 ts=4 sts=4

" Enable per-project configuration
set exrc
set secure
