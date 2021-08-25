set breakindent
set confirm
set cursorline
set expandtab
set ignorecase
set lazyredraw
set linebreak
set pumheight=10
set scrolloff=2
set shell=/bin/sh
set shiftwidth=4
set showmatch
set sidescrolloff=5
set smartcase
set noswapfile
set tabline=%!tabline#show()
set tags^=./.git/tags;
set tagcase=match
set undofile
set updatetime=100
set wildignore+=*.pyc,__pycache__,.DS_Store,*~,#*#
set wildignorecase
set wildmode=longest:full,full

let &colorcolumn = '+' . join(range(1, 256), ',+')
let &statusline = ' %{statusline#obsession()}%<%f [%{&filetype ==# "" ? "none" : &filetype}] %m%r%=%{statusline#git()}%14.(%l:%c%V%)%14.P '

set grepformat^=%f:%l:%c:%m
if executable('rg')
  set grepprg=rg\ --vimgrep\ --smart-case
else
  set grepprg=grep\ --line-number\ --recursive\ -I\ $*\ /dev/null
endif

" Enable indent-heuristic to make vimdiff more closely match git diff
set diffopt+=indent-heuristic

" Auto close braces in insert mode
inoremap {<CR> {<CR>}<Esc>O

" Make * and # work in visual mode
xnoremap * y/\V<C-R>"<CR>
xnoremap # y?\V<C-R>"<CR>

nnoremap <expr> j (v:count == 0 ? 'gj' : 'j')
nnoremap <expr> k (v:count == 0 ? 'gk' : 'k')
nnoremap / ms/
nnoremap ? ms?
nnoremap <Space>w <Cmd>w<CR>
nnoremap <Space>b :ls<CR>:b<Space>
nnoremap <C-W><Space>b :ls<CR>:sb<Space>
nnoremap <Space>e :e %:p:h/<Tab>
nnoremap <C-W><Space>e :sp %:p:h/<Tab>
nnoremap <Space>t :tjump /
nnoremap <C-W><Space>t :stjump /

" TODO Remove in nvim 0.6
set hidden
set nojoinspaces
set inccommand=nosplit
set viewoptions-=options
inoremap <C-U> <C-G>u<C-U>
inoremap <C-W> <C-G>u<C-W>
nnoremap Q gq
nnoremap Y y$
nnoremap <C-L> <Cmd>nohlsearch<Bar>diffupdate<Bar>syn sync minlines=50<CR><C-L>

" Format whole buffer with formatprg without changing cursor position
" See :h restore-position
nnoremap gq<CR> mzHmygggqG`yzt`z

" Unimpaired style mappings
nnoremap <expr> [a '<Cmd>' . v:count1 . 'prev<CR>'
nnoremap <expr> ]a '<Cmd>' . v:count1 . 'next<CR>'
nnoremap <expr> [b '<Cmd>' . v:count1 . 'bprev<CR>'
nnoremap <expr> ]b '<Cmd>' . v:count1 . 'bnext<CR>'
nnoremap <expr> [l '<Cmd>' . v:count1 . 'lprev<CR>'
nnoremap <expr> ]l '<Cmd>' . v:count1 . 'lnext<CR>'
nnoremap <expr> [<C-L> '<Cmd>' . v:count1 . 'lolder<CR>'
nnoremap <expr> ]<C-L> '<Cmd>' . v:count1 . 'lnewer<CR>'
nnoremap <expr> [q '<Cmd>' . v:count1 . 'cprev<CR>'
nnoremap <expr> ]q '<Cmd>' . v:count1 . 'cnext<CR>'
nnoremap <expr> [<C-Q> '<Cmd>' . v:count1 . 'colder<CR>'
nnoremap <expr> ]<C-Q> '<Cmd>' . v:count1 . 'cnewer<CR>'
nnoremap <expr> [t '<Cmd>' . v:count1 . 'tprev<CR>'
nnoremap <expr> ]t '<Cmd>' . v:count1 . 'tnext<CR>'
nnoremap [A <Cmd>first<CR>
nnoremap ]A <Cmd>last<CR>
nnoremap [B <Cmd>bfirst<CR>
nnoremap ]B <Cmd>blast<CR>
nnoremap [L <Cmd>lfirst<CR>
nnoremap ]L <Cmd>llast<CR>
nnoremap [Q <Cmd>cfirst<CR>
nnoremap ]Q <Cmd>clast<CR>
nnoremap [T <Cmd>tfirst<CR>
nnoremap ]T <Cmd>tlast<CR>
nnoremap <expr> [e '<Cmd>.move --' . v:count1 . '<CR>'
nnoremap <expr> ]e '<Cmd>.move +' . v:count1 . '<CR>'
xnoremap <expr> [e ':move --' . v:count1 . '<CR>gv'
xnoremap <expr> ]e ':move +' . (v:count1 + line('''>') - line('''<')) . '<CR>gv'
nnoremap [<Space> <Cmd>put! =repeat(nr2char(10), v:count1)<CR><CR>:']+1<CR>
nnoremap ]<Space> <Cmd>put =repeat(nr2char(10), v:count1)<CR><CR>:'[-1<CR>
nnoremap yon <Cmd>setlocal number!<Bar>set nu?<CR>
nnoremap yor <Cmd>setlocal relativenumber!<Bar>set rnu?<CR>
nnoremap yol <Cmd>setlocal list!<Bar>set list?<CR>
nnoremap yoc <Cmd>setlocal cursorline!<Bar>set cul?<CR>
nnoremap yo<Bar> <Cmd>setlocal cursorcolumn!<Bar>set cuc?<CR>
nnoremap <expr> yod '<Cmd>' . (&diff ? 'diffoff' : 'diffthis') . '<CR>'
nnoremap yos <Cmd>setlocal spell!<Bar>set spell?<CR>

nnoremap m<CR> <Cmd>make<CR>
nnoremap m<Space> :make<Space>
nnoremap m? <Cmd>set makeprg?<CR>

function! Sort(type, ...) abort
    '[,']sort
    call setpos('.', getpos("''"))
endfunction
nnoremap gs m'<Cmd>set operatorfunc=Sort<CR>g@
xnoremap gs :sort<CR>

cnoremap <expr> <C-P> wildmenumode() ? '<C-P>' : '<Up>'
cnoremap <expr> <C-N> wildmenumode() ? '<C-N>' : '<Down>'
cnoremap <expr> <C-J> pumvisible() ? '<Down><Tab>' : '<C-J>'
cnoremap <expr> <C-K> pumvisible() ? '<Up><Tab>' : '<C-K>'

augroup init

autocmd! BufWinEnter * if &previewwindow | nnoremap <buffer> q <C-W>q | endif
autocmd! CmdWinEnter * nnoremap <buffer> q <C-W>q
autocmd! TextYankPost * lua vim.highlight.on_yank {higroup="Visual", timeout=150, on_visual=true}
autocmd! TermOpen * setlocal statusline=%{b:term_title} | startinsert
autocmd! InsertEnter,WinLeave,FocusLost * setlocal nocursorline
autocmd! InsertLeave,WinEnter,FocusGained * if mode() !=# 'i' | let &l:cursorline = 1 | endif
autocmd! BufRead * autocmd FileType <buffer> ++once if &ft !~# 'commit\|rebase' | exec 'silent! normal! g`"' | endif
autocmd! BufNewFile * autocmd BufWritePre <buffer> ++once call mkdir(expand('%:h'), 'p')
autocmd! UIEnter * colorscheme base16-eighties

augroup END
