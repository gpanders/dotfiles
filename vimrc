" Plugins {{{
" Specify a directory for plugins
call plug#begin(has('nvim') ? '~/.config/nvim/plugins' : '~/.vim/bundle')

Plug 'artur-shaik/vim-javacomplete2'
Plug 'altercation/vim-colors-solarized'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-capslock'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'vim-utils/vim-husk'
Plug 'itchyny/lightline.vim'
Plug 'mgee/lightline-bufferline'
Plug 'qpkorr/vim-bufkill'
Plug 'easymotion/vim-easymotion'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" nvim specific plugins
if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'Shougo/neco-syntax'
    Plug 'zchee/deoplete-clang'
    Plug 'arakashic/chromatica.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'neomake/neomake'
endif

" Initialize plugin system
call plug#end()
" }}}

" Set the leader key
" This line needs to be before the plugin configuration files so that the
" config files can also use the <leader> value
let mapleader = ','

" 
if has('nvim')
    runtime! config/**/*.vim
endif

" Set solarized color scheme
set background=dark
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
nnoremap <M-j> <C-w>-
nnoremap <M-k> <C-w>+
nnoremap <M-h> <C-w><
nnoremap <M-l> <C-w>>

" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

" vim: foldmethod=marker
