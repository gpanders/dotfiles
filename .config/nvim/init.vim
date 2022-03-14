" Ensure filetype autocommands are created first
filetype on

set breakindent
set colorcolumn=+1
set completeopt=menu,menuone
set confirm
set cursorline
set expandtab
set foldlevelstart=99
set ignorecase
set lazyredraw
set linebreak
set list
set pumheight=10
set scrolloff=2
set shada='100,<50,s10,:100,/100,h
set shell=/bin/sh
set shiftwidth=4
set shortmess+=c
set sidescrolloff=5
set smartcase
set noswapfile
set tabline=%!tabline#show()
set tagcase=match
set undofile
set wildignore+=*.pyc,__pycache__,.DS_Store,*~,#*#,*.o
set wildignorecase
set wildmode=longest:full,full

if executable('rg')
  set grepprg=rg\ --vimgrep\ --smart-case
  set grepformat=%f:%l:%c:%m
else
  set grepprg=grep\ --line-number\ --recursive\ -I\ $*
  set grepformat=%f:%l:%m
  if has('mac')
    " BSD grep that ships with macOS reads from stdin when no arguments are
    " provided, so append /dev/null to prevent it from blocking
    set grepprg+=\ /dev/null
  endif
endif

" Enable indent-heuristic to make vimdiff more closely match git diff
set diffopt+=indent-heuristic

" Auto close braces in insert mode
inoremap {<CR> {<CR>}<Esc>O

inoremap <C-Space> <C-X><C-O>

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
nnoremap <Space>t :Tjump /

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

nnoremap <expr> <Space>q getqflist(#{winid: 1}).winid ? '<Cmd>cclose<CR>' : '<Cmd>copen<CR><C-W>p'
nnoremap <expr> <Space>l getloclist(0, #{winid: 1}).winid ? '<Cmd>lclose<CR>' : '<Cmd>lopen<CR><C-W>p'

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
  autocmd!

  " Close preview and command windows with q
  autocmd BufWinEnter * if &previewwindow | nnoremap <buffer> q <C-W>q | endif
  autocmd CmdWinEnter * nnoremap <buffer> q <C-W>q

  " Highlight yanked text
  autocmd TextYankPost * lua vim.highlight.on_yank {higroup="Visual", timeout=150, on_visual=true}

  " Start insert mode in terminals automatically (and set the statusline to
  " the terminal title)
  autocmd TermOpen * setlocal statusline=%{b:term_title} | startinsert

  " Auto close shell terminals (#15440)
  autocmd TermClose *
        \ if !v:event.status |
        \   let info = nvim_get_chan_info(&channel) |
        \   if get(info, 'argv', []) ==# [&shell] |
        \     exec 'bdelete! ' .. expand('<abuf>') |
        \   endif |
        \ endif

  " Hide cursorline in insert mode and when the current window doesn't have
  " focus
  autocmd InsertEnter,WinLeave,FocusLost * setlocal nocursorline
  autocmd InsertLeave,WinEnter,FocusGained * if mode() !=# 'i' | let &l:cursorline = 1 | endif

  " Create missing parent directories automatically
  autocmd BufNewFile * autocmd BufWritePre <buffer> ++once call mkdir(expand('%:h'), 'p')

  " Defer setting the colorscheme until the UI loads (micro optimization)
  autocmd UIEnter * silent! colorscheme dark

  " Restore cursor position (except for git commits and rebases)
  autocmd BufRead * if &ft !~# 'commit\|rebase' | exec 'silent! normal! g`"' | endif

  " Don't show trailing spaces in insert mode
  autocmd InsertEnter * setlocal listchars-=trail:-
  autocmd InsertLeave * setlocal listchars+=trail:-
augroup END
