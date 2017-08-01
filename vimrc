" Plugins {{{
" Specify a directory for plugins
call plug#begin(has('nvim') ? '~/.config/nvim/plugins' : '~/.vim/bundle')

Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-capslock'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'altercation/vim-colors-solarized'
Plug 'airblade/vim-gitgutter'
Plug 'vim-utils/vim-husk'
Plug 'itchyny/lightline.vim'
Plug 'taohex/lightline-buffer'
Plug 'qpkorr/vim-bufkill'
Plug 'easymotion/vim-easymotion'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" Language specific plugins
Plug 'Rip-Rip/clang_complete', { 'for': ['c', 'cpp'] }
Plug 'octol/vim-cpp-enhanced-highlight', { 'for': ['c', 'cpp'] }
Plug 'artur-shaik/vim-javacomplete2', { 'for': 'java' }

if has('nvim')
    " Neovim specific plugins
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'neomake/neomake'
else
    " Vim specific plugins
    Plug 'tpope/vim-sensible'
    Plug 'Shougo/neocomplete.vim'
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
set tabstop=4
set shiftwidth=4
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

" Navigate between windows easier
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-h> <C-w>h
map <C-l> <C-w>l

" Simplify resizing splits
if has('nvim')
  nnoremap <A-j> <C-w>-
  nnoremap <A-k> <C-w>+
  nnoremap <A-h> <C-w><
  nnoremap <A-l> <C-w>>
else
  nnoremap j <C-w>-
  nnoremap k <C-w>+
  nnoremap h <C-w><
  nnoremap l <C-w>>
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

" vim: foldmethod=marker
