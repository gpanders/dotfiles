" Source plugins list
source $HOME/.vim/plugins.vim

" Set the leader key
let mapleader = ','
let maplocalleader = '\'

" Set color scheme {{{
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set termguicolors
set background=light
" let g:solarized_termtrans=1
colorscheme solarized8
" }}}

" Sudo save
cnoreabbrev w!! w !sudo tee > /dev/null %

" Settings {{{
set tabstop=2              " Tab width
set softtabstop=2          " Tab width
set shiftwidth=2           " Tab width
set expandtab              " Use spaces instead of tabs
set number                 " Show line numbers
set hidden                 " Hide buffers instead of closing them
set ignorecase             " Make searching case insensitive
set smartcase              " ... unless the query has capital letters
set gdefault               " Use 'g' flag by default with :s/foo/bar/.
set magic                  " Use 'magic' patterns (extended regular expressions)
set hlsearch               " Highlight search results
set splitbelow             " Open horizontal splits below current pane
set splitright             " Open vertical splits to the right of current pane
set nostartofline          " Don't move cursor to start of line when scrolling
set scrolloff=2            " 2 lines above/below cursor when scrolling
set showmatch              " Show matching bracket in insert mode
set matchtime=2            " Show matching bracket for 2 tenths of a second
set matchpairs+=<:>        " Add angle brackets as match pairs
set colorcolumn=80         " Show column at 80 characters
set noswapfile             " Disable swap file
set foldmarker=\ {{{,\ }}} " Make default fold marker have a space

" Show completion menu even when only one match
" set completeopt=menuone,preview
" }}}

" Don't press Shift to enter command mode
" map ; :

" Insert mode mappings {{{

" jk in Insert mode escapes to Normal mode
inoremap jk <Esc>

" Auto close braces in insert mode
inoremap {<CR> {<CR>}<Esc>ko

" Use <C-W> in inset mode to make the word under the cursor all caps
inoremap <C-U> <Esc>gUiw`]a

" Delete forward in insert mode
inoremap <C-L> <Del>
" }}}

" Normal mode mappings {{{

" Save by pressing <leader>w
nnoremap <silent> <leader>w :w<CR>

" Map - to default behavior of , (which is now the mapleader)
nnoremap - ,

" Use :tjump by default
nnoremap <C-]> g<C-]>

" Use \\ in Normal mode to use grepprg
" Define a custom command to wrap :grep with :silent, :cwindow, and :redraw
command! -nargs=+ -complete=file -bar -bang Grep silent! grep<bang> <args>|cwindow|redraw!
nnoremap \\ :Grep!<space>

" Buffer shortcuts {{{
nnoremap <silent> <leader>1 :b1<CR>
nnoremap <silent> <leader>2 :b2<CR>
nnoremap <silent> <leader>3 :b3<CR>
nnoremap <silent> <leader>4 :b4<CR>
nnoremap <silent> <leader>5 :b5<CR>
nnoremap <silent> <leader>6 :b6<CR>
nnoremap <silent> <leader>7 :b7<CR>
nnoremap <silent> <leader>8 :b8<CR>
nnoremap <silent> <leader>9 :b9<CR>
nnoremap <silent> <leader>0 :b10<CR>

" List buffers and put :b on the command line
noremap <leader>ls :ls<CR>:b

noremap <C-W>c :bd<CR>

noremap <silent> <M-[> :bprev<CR>
noremap <silent> <M-]> :bnext<CR>
" }}}

" Augment [I and ]I to place an :ij prompt below results for easy jumping
noremap [I [I:ij  <C-R><C-W><S-Left><Left>
noremap ]I ]I:ij  <C-R><C-W><S-Left><Left>

" Augment [D and ]D to place an :dj prompt below results for easy jumping
noremap [D [D:dj  <C-R><C-W><S-Left><Left>
noremap ]D ]D:dj  <C-R><C-W><S-Left><Left>

" Open jumplist
map <leader>ju :jumps<CR>

" Open quickfix list and place :cc on the prompt
noremap <leader>cl :clist<CR>:sil cc<space>

" Open location list and place :ll on the prompt
noremap <leader>ll :llist<CR>:sil ll<space>

" Open undo list
map <leader>u :undol<CR>

" Show marks
map <leader>m :marks<CR>

" <leader>ev opens .vimrc in new window
nnoremap <leader>ev :vsplit $MYVIMRC<CR>

" <leader>sv sources .vimrc
nnoremap <leader>sv :source $MYVIMRC<CR>

" Toggle between normal and relative numbering
nnoremap <leader>r :call NumberToggle()<CR>

" Navigate through wrapped lines individually
nnoremap <silent> j gj
nnoremap <silent> k gk

" Use space bar to toggle folds
nnoremap <space> za

" Keep search matches in the middle of the window
" nnoremap * *zzzv
" nnoremap # #zzzv
" nnoremap n nzzzv
" nnoremap N Nzzzv

" Keep jumps in the middle of the window
" nnoremap g, g,zz
" nnoremap g; g;zz

" Clear search buffer with <C-N>
nnoremap <silent> <C-N> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>

" Simplify resizing splits
" nnoremap <silent> ^[j :resize +1<CR>
" nnoremap <silent> ^[k :resize -1<CR>
" nnoremap <silent> ^[h :vertical resize -1<CR>
" nnoremap <silent> ^[l :vertical resize +1<CR>
" }}}

" Search for visual selection with g/
vnoremap g/ y/\V<C-R>"<CR>

" Use ag as grepprg if available
if executable('ag')
    " Use ag over grep
    set grepprg=ag\ --vimgrep
    set grepformat='%f:%l:%c:%m,%f:%l:%m'
endif

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

" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has('autocmd')
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$")
        \ | exe "normal! g`\"" | endif
endif

" Pushing built-in commands beyond their limits {{{
" https://gist.github.com/Konfekt/d8ce5626a48f4e56ecab31a89449f1f0
function! <sid>CCR()
    if getcmdtype() isnot# ':'
      return "\<CR>"
    endif
    let cmdline = getcmdline()
    if cmdline =~# '\v^\s*(ls|files|buffers)!?\s*(\s[+\-=auhx%#]+)?$'
        " like :ls but prompts for a buffer command
        return "\<CR>:b"
    elseif cmdline =~# '\v/(#|nu%[mber])$'
        " like :g//# but prompts for a command
        return "\<CR>:"
    elseif cmdline =~# '\v^\s*(dli%[st]|il%[ist])!?\s+\S'
        " like :dlist or :ilist but prompts for a count for :djump or :ijump
        return "\<CR>:" . cmdline[0] . "j  " . split(cmdline, " ")[1] . "\<S-Left>\<Left>"
    elseif cmdline =~# '\v^\s*(cli|lli)%[st]!?\s*(\s\d+(,\s*\d+)?)?$'
        " like :clist or :llist but prompts for an error/location number
        return "\<CR>:sil " . repeat(cmdline[0], 2) . "\<Space>"
    elseif cmdline =~# '\v^\s*ol%[dfiles]\s*$'
        " like :oldfiles but prompts for an old file to edit
        set nomore
        return "\<CR>:sil se more|e #<"
    elseif cmdline =~# '^\s*changes\s*$'
        " like :changes but prompts for a change to jump to
        set nomore
        return "\<CR>:sil se more|norm! g;\<S-Left>"
    elseif cmdline =~# '\v^\s*ju%[mps]'
        " like :jumps but prompts for a position to jump to
        set nomore
        return "\<CR>:sil se more|norm! \<C-o>\<S-Left>"
    elseif cmdline =~ '\v^\s*marks\s*(\s\w+)?$'
        " like :marks but prompts for a mark to jump to
        return "\<CR>:norm! `"
    elseif cmdline =~# '\v^\s*undol%[ist]'
        " like :undolist but prompts for a change to undo
        return "\<CR>:u "
    else
        return "\<c-]>\<CR>"
    endif
endfunction
cnoremap <expr> <CR> <sid>CCR()
" }}}

" File-type specific configuration {{{
augroup vimrcft
  autocmd!
  " C / C++
  au FileType c,cpp setlocal sw=2 ts=2 sts=2 cms=//%s cin

  " CMake
  au FileType cmake setlocal cms=#%s

  " Python
  au FileType python setlocal sw=4 ts=4 sts=4 kp=pydoc

  " LaTeX
  au FileType tex setlocal nocursorline norelativenumber
  au FileType tex :NoMatchParen

  " Vim
  au FileType vim setlocal fdm=marker
augroup END
" }}}

" Enable per-project configuration {{{
set exrc
set secure
" }}}

" Source plugin configuration files
" Do this last so that plugins can override default configuration
runtime! config/*.vim
