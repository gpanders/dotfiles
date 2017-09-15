" Plugins {{{
" Specify a directory for plugins
call plug#begin(has('nvim') ? '~/.config/nvim/plugins' : '~/.vim/bundle')

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
Plug 'altercation/vim-colors-solarized'

" Show git status icons in gutter
Plug 'airblade/vim-gitgutter'

" Use navigation keybindings in command mode
Plug 'vim-utils/vim-husk'

" Use same bindings to navigate between vim splits and tmux panes
Plug 'christoomey/vim-tmux-navigator'

" Easy motion
Plug 'easymotion/vim-easymotion'

" Show marks in gutter
Plug 'kshenoy/vim-signature'

" Automatically update tags file
Plug 'ludovicchabant/vim-gutentags'

" Lightline (more lightweight version of vim-airline)
Plug 'itchyny/lightline.vim'
Plug 'taohex/lightline-buffer'

" Fix vim fold updating for better performance
Plug 'Konfekt/FastFold'

" Maintain ideal buffer size for active buffer
Plug 'roman/golden-ratio'

" FZF
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" Snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

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

" LaTeX
Plug 'lervag/vimtex', { 'for': 'tex' }

if has('nvim')
  " Neovim specific plugins
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'tweekmonster/deoplete-clang2', { 'for': ['c', 'cpp'] }
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

if has('nvim')
  " Neovim specific configuration

  " Source nvim configs
  runtime! config/**/*.vim

  " Open search results in split window
  set inccommand=split
else
  " Vim specific configuration
endif

" Set the leader key
let mapleader = ','
let maplocalleader = '\'

" Set color scheme
set background=light
colorscheme solarized

" Save by pressing <leader>w
nmap <silent> <leader>w :w<CR>

" Some good default options {{{
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set number
set hidden              " Hide buffers instead of closing them
set ignorecase          " Make searching case insensitive
set smartcase           " ... unless the query has capital letters
set gdefault            " Use 'g' flag by default with :s/foo/bar/.
set magic               " Use 'magic' patterns (extended regular expressions)
set hlsearch
set splitbelow          " Open horizontal splits below current pane
set splitright          " Open vertical splits to the right of current pane
set nostartofline       " Don't move cursor to start of line when scrolling
set scrolloff=2         " 2 lines above/below cursor when scrolling
set showmatch           " Show matching bracket in insert mode
set matchtime=2         " Show matching bracket for 2 tenths of a second
set matchpairs+=<:>     " Add angle brackets as match pairs
set colorcolumn=80      " Show column at 80 characters
" }}}

" Don't press Shift to enter command mode
map ; :

" Auto close braces in insert mode
inoremap {<CR> {<CR>}<Esc>ko

" Set update time
set updatetime=250

" Disable swap file
set noswapfile

" Make default fold marker have a space
set foldmarker=\ {{{,\ }}}

" Show completion menu even when only one match
set completeopt=menuone,preview

" Delete forward in insert mode
inoremap <C-L> <Del>

" Search for visual selection with g/
vnoremap g/ y/\V<C-R>"<CR>

" Use :tjump by default
nnoremap <C-]> g<C-]>

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

" Keep search matches in the middle of the window
nnoremap * *zzzv
nnoremap # #zzzv
nnoremap n nzzzv
nnoremap N Nzzzv

" Keep jumps in the middle of the window
nnoremap g, g,zz
nnoremap g; g;zz

" Highlight the current line in the current window but disable in Insert mode
aug cursorline
  let blacklist = ['tex']
  au!
  au BufEnter * if index(blacklist, &ft) < 0 | set cursorline | endif
  au BufLeave * if index(blacklist, &ft) < 0 | set nocursorline | endif
  au InsertEnter * if index(blacklist, &ft) < 0 | set nocursorline | endif
  au InsertLeave * if index(blacklist, &ft) < 0 | set cursorline | endif
aug END

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


" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has('autocmd')
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$")
        \ | exe "normal! g`\"" | endif
endif

" File-type specific configuration
" C / C++
au FileType c,cpp setlocal sw=2 ts=2 sts=2 cms=//%s

" CMake
au FileType cmake setlocal cms=#%s

" Python
au FileType python setlocal sw=4 ts=4 sts=4

" LaTeX
au FileType tex setlocal nocursorline norelativenumber
au FileType tex :NoMatchParen

" Vim
au FileType vim setlocal fdm=marker

" Enable per-project configuration
set exrc
set secure
