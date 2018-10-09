" Plugins {{{
" Specify a directory for plugins
call plug#begin('~/.vim/plug')

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

" Lightline (more lightweight version of vim-airline)
Plug 'itchyny/lightline.vim'
" Plug 'taohexxx/lightline-buffer'
Plug 'mgee/lightline-bufferline'

" Fix vim fold updating for better performance
Plug 'Konfekt/FastFold'

" Maintain ideal buffer size for active buffer
Plug 'roman/golden-ratio'

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
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'zchee/deoplete-clang', { 'for': ['c', 'cpp'] }
  Plug 'zchee/deoplete-jedi', { 'for': 'python' }
else
  " Vim specific plugins
  Plug 'tpope/vim-sensible'
  Plug 'justmao945/vim-clang', { 'for': ['c', 'cpp'] }
endif

" Initialize plugin system
call plug#end()

" Source plugin configuration files
runtime! config/*.vim
" }}}

" Set the leader key
let mapleader = ','
let maplocalleader = '\'

" Set color scheme
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set termguicolors
set background=light
" let g:solarized_termtrans=1
colorscheme solarized8

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
" map ; :

" jj in Insert mode escapes to Normal mode
imap jj <Esc>

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

" Use \\ in Normal mode to use grepprg
nnoremap \\ :grep <space>

" Use ag as grepprg if available
if executable('ag')
    " Use ag over grep
    set grepprg=ag\ --vimgrep
    set grepformat=%f:%l:%c:%m
endif

" Buffer shortcuts {{{
nmap <silent> <leader>1 :b1<CR>
nmap <silent> <leader>2 :b2<CR>
nmap <silent> <leader>3 :b3<CR>
nmap <silent> <leader>4 :b4<CR>
nmap <silent> <leader>5 :b5<CR>
nmap <silent> <leader>6 :b6<CR>
nmap <silent> <leader>7 :b7<CR>
nmap <silent> <leader>8 :b8<CR>
nmap <silent> <leader>9 :b9<CR>
nmap <silent> <leader>0 :b10<CR>

map <C-W>c :bd<CR>

map <silent> <M-[> :bprev<CR>
map <silent> <M-]> :bnext<CR>
" }}}

" Relative numbering {{{
function! NumberToggle()
  if (&relativenumber == 1)
    set nornu
    set number
  else
    set rnu
  endif
endfunc
" }}}

" Toggle between normal and relative numbering
nnoremap <leader>r :call NumberToggle()<CR>

" Navigate through wrapped lines individually {{{
nnoremap <silent> j gj
nnoremap <silent> k gk
" }}}

" Keep search matches in the middle of the window {{{
nnoremap * *zzzv
nnoremap # #zzzv
nnoremap n nzzzv
nnoremap N Nzzzv
" }}}

" Keep jumps in the middle of the window {{{
nnoremap g, g,zz
nnoremap g; g;zz
" }}}

" Highlight the current line in the current window but disable in Insert mode {{{
" aug cursorline
"   let blacklist = ['tex']
"   au!
"   au BufEnter * if index(blacklist, &ft) < 0 | set cursorline | endif
"   au BufLeave * if index(blacklist, &ft) < 0 | set nocursorline | endif
"   au InsertEnter * if index(blacklist, &ft) < 0 | set nocursorline | endif
"   au InsertLeave * if index(blacklist, &ft) < 0 | set cursorline | endif
" aug END
" }}}

" Clear search buffer with <C-N>
nmap <silent> <C-N> :nohlsearch<CR>

" Simplify resizing splits {{{
nnoremap <silent> ^[j :resize +1<CR>
nnoremap <silent> ^[k :resize -1<CR>
nnoremap <silent> ^[h :vertical resize -1<CR>
nnoremap <silent> ^[l :vertical resize +1<CR>
" }}}

" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has('autocmd')
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$")
        \ | exe "normal! g`\"" | endif
endif

" File-type specific configuration {{{
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
" }}}

" Enable per-project configuration {{{
set exrc
set secure
" }}}

