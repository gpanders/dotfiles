" Source plugins list
source $HOME/.vim/plugins.vim

" Set the leader key
let mapleader = ','
let maplocalleader = '\'

" Set color scheme {{{
" Set background if $TERMBG is set, otherwise let vim decide
if $TERMBG == 'light'
  set background=light
elseif $TERMBG == 'dark'
  set background=dark
endif

if has('termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
else
  let g:solarized_use16 = 1
  let g:onedark_termcolors = 16

endif
colorscheme onedark
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
set modeline               " Enable vim modelines

set path=.,**              " Add all subdirectories to path
set wildcharm=<C-Z>
set wildignorecase
set wildignore=*.swp,*.bak
set wildignore+=*.pyc,*.class,*.sln,*.Master,*.csproj,*.csproj.user,*.cache,*.dll,*.pdb,*.min.*
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=tags
set wildignore+=*.tar.*
" }}}

" Statusline {{{
" Set the statusline
function! StatuslineGitBranch()
  let branch = FugitiveHead()
  if branch != ""
    return " " . branch
  else
    return ""
  endif
endfunction

set laststatus=2
set statusline=                              " Reset the statusline
set statusline+=%1*\ %<%f\ %h%m%r\           " Filename and help/modified/RO markers
set statusline+=%2*\ %n\                     " Buffer number
set statusline+=%3*                          " Reset color
set statusline+=\ %{CapsLockStatusline()}\   " Show caps lock indicator
set statusline+=%=                           " Break point for right justify
set statusline+=\ %{StatuslineGitBranch()}\  " Git branch (with icon)
set statusline+=\ %{&ft}\                    " Filetype
set statusline+=%4*\ %P\                     " Percent through file
set statusline+=%5*\ %(%l:%c%V%)\            " Line and column number

" Set User1-9 highlight groups for statusline
" These are used by adding %N* to the statusline, where {N} is 1-9
" The following uses the solarized color scheme
if g:colors_name ==# 'solarized8'
  if &background ==# 'light'
    hi User1 ctermfg=15 ctermbg=11 guifg=#fdf6e3 guibg=#657b83
    hi User2 ctermfg=15 ctermbg=14 guifg=#fdf6e3 guibg=#93a1a1
    hi User3 ctermfg=11 ctermbg=7 guifg=#657b83 guibg=#eee8d5
    hi User4 ctermfg=15 ctermbg=14 guifg=#fdf6e3 guibg=#93a1a1
    hi User5 ctermfg=15 ctermbg=11 guifg=#fdf6e3 guibg=#657b83
  else
    hi User1 ctermfg=8 ctermbg=12 guifg=#002b36 guibg=#839496
    hi User2 ctermfg=8 ctermbg=10 guifg=#002b36 guibg=#586e75
    hi User3 ctermfg=12 ctermbg=0 guifg=#839496 guibg=#073642
    hi User4 ctermfg=8 ctermbg=10 guifg=#002b36 guibg=#586e75
    hi User5 ctermfg=8 ctermbg=12 guifg=#002b36 guibg=#839496
  endif
elseif g:colors_name ==# 'onedark'
  hi User1 cterm=reverse gui=reverse
  hi User2 ctermbg=237 guibg=#3e4452
  hi User3 ctermfg=145 ctermbg=235 guifg=#abb2bf guibg=#282c34
  hi User4 ctermbg=237 guibg=#3e4452
  hi User5 cterm=reverse gui=reverse
endif
" }}}

" Insert mode mappings {{{

" jk in Insert mode escapes to Normal mode
inoremap jk <Esc>l

" Auto close braces in insert mode
inoremap {<CR> {<CR>}<Esc>ko

" Use <C-U> in insert mode to make the word under the cursor all caps
inoremap <C-U> <Esc>gUiw`]a

" Delete forward in insert mode
inoremap <C-L> <Del>
" }}}

" Normal mode mappings {{{

" Save by pressing <leader>w
nnoremap <silent> <leader>w :w<CR>

" Make :tjump easier
nnoremap g] g<C-]>

" Map Q to gq
noremap Q gq

" Use <C-K> in Normal mode to use grepprg
" Define a custom command to wrap :grep with :silent, :copen, and :redraw
command! -nargs=+ -complete=file -bar -bang Grep silent! grep<bang> <args>|copen|redraw!
nnoremap <C-K> :Grep!<space>

" Buffer shortcuts
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
noremap <leader>b :buffers<CR>:b <C-Z><S-Tab>
noremap <leader>B :buffers<CR>:sb <C-Z><S-Tab>

" Augment [I and ]I to place an :ij prompt below results for easy jumping
noremap [I [I:ij  <C-R><C-W><S-Left><Left>
noremap ]I ]I:ij  <C-R><C-W><S-Left><Left>

" Augment [D and ]D to place an :dj prompt below results for easy jumping
noremap [D [D:dj  <C-R><C-W><S-Left><Left>
noremap ]D ]D:dj  <C-R><C-W><S-Left><Left>

" Open quickfix list and place :cc on the prompt
noremap <leader>cl :clist<CR>:sil cc<space>

" Open location list and place :ll on the prompt
noremap <leader>ll :llist<CR>:sil ll<space>

nnoremap <leader>f :find *
nnoremap <leader>t :tabfind *
nnoremap <leader>j :tjump /
nnoremap <leader>p :ptjump /

nnoremap <leader>F :find <C-R>=expand('%:h').'/*'<CR>
nnoremap <leader>T :tabfind <C-R>=expand('%:h').'/*'<CR>

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

" Clear search buffer with <C-N>
nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>

" }}}

" Visual mode mappings {{{
vnoremap * y/\V<C-R>"<CR>
vnoremap # y?\V<C-R>"<CR>
" }}}

" Use ripgrep or ag as grepprg if available
if executable('rg')
  " Use rg over grep
  set grepprg=rg\ --vimgrep\ --hidden
elseif executable('ag')
    " Use ag over grep
    set grepprg=ag\ --vimgrep\ --hidden
endif
set grepformat='%f:%l:%c:%m,%f:%l:%m'

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

" Autocommands {{{
augroup vimrc
  autocmd!
  " Have Vim jump to the last position when reopening a file
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$")
        \ | exe "normal! g`\"" | endif

  " Highlight the current line in the current window but disable in Insert mode {{{
  let blacklist = ['tex']
  au BufEnter * if index(blacklist, &ft) < 0 | set cursorline | endif
  au BufLeave * if index(blacklist, &ft) < 0 | set nocursorline | endif
  au InsertEnter * if index(blacklist, &ft) < 0 | set nocursorline | endif
  au InsertLeave * if index(blacklist, &ft) < 0 | set cursorline | endif
  " }}}

  " Open the quickfix window after any quickfix command
  au QuickFixCmdPost [^l]* cwindow

  " term
  if has('nvim')
    au TermOpen * startinsert
  endif

  " Press q to close certain windows
  au FileType qf nnoremap <silent> <buffer> q :q<CR>
  au FileType netrw nnoremap <silent> <buffer> qq :Rex<CR>

augroup END
" }}}

" Enable per-project configuration {{{
set exrc
set secure
" }}}
