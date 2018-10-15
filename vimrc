" Source plugins list
source $HOME/.vim/plugins.vim

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

" Sudo save
cmap w!! w !sudo tee > /dev/null %

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
nnoremap \\ :grep<space>

" Use ag as grepprg if available
if executable('ag')
    " Use ag over grep
    set grepprg=ag\ --vimgrep
    set grepformat='%f:%l:%c:%m,%f:%l:%m'
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
aug cursorline
  let blacklist = ['tex']
  au!
  au BufEnter * if index(blacklist, &ft) < 0 | set cursorline | endif
  au BufLeave * if index(blacklist, &ft) < 0 | set nocursorline | endif
  au InsertEnter * if index(blacklist, &ft) < 0 | set nocursorline | endif
  au InsertLeave * if index(blacklist, &ft) < 0 | set cursorline | endif
aug END
" }}}

" Clear search buffer with <C-N>
nnoremap <silent> <C-N> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>

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

" Source plugin configuration files
" Do this last so that plugins can override default configuration
runtime! config/*.vim
