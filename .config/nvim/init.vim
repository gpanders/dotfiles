" Ensure filetype autocommands are created first
filetype on

colorscheme nord

set breakindent
set cinoptions=l1,:0,g0,E-s,N-s,t0,(s,J1,j1
set colorcolumn=+1
set completeopt=menu,menuone
set confirm
set cursorline
set expandtab
set exrc
set foldlevelstart=99
set ignorecase
set jumpoptions=view
set lazyredraw
set linebreak
set list
set path=.,,
set pumheight=10
set scrolloff=2
set shada='100,<50,s10,:100,/100,h,r/tmp/,r/private/,rfugitive:,rzipfile:,rterm:
set shell=/bin/sh
set shiftwidth=4
set sidescrolloff=5
set smartcase
set splitright
set statusline=%{%v:lua.require'statusline'.statusline()%}
set noswapfile
set tabline=%!v:lua.require'statusline'.tabline()
set tagcase=match
set termguicolors
set undofile
set updatetime=250
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
set diffopt+=indent-heuristic,linematch:60

inoremap <C-Space> <C-X><C-O>

nnoremap <expr> j (v:count == 0 ? 'gj' : 'j')
nnoremap <expr> k (v:count == 0 ? 'gk' : 'k')
nnoremap / ms/
nnoremap ? ms?
nnoremap <Space>w <Cmd>update<CR>
nnoremap <Space>b :ls<CR>:b<Space>
nnoremap <Space>ee :e %:p:h/<Tab>
nnoremap <Space>es :sp %:p:h/<Tab>
nnoremap <Space>ev :vs %:p:h/<Tab>
nnoremap <Space>y "+y
xnoremap <Space>y "+y

" Format whole buffer with formatprg without changing cursor position
nnoremap gq<CR> mzgggqG`z
nnoremap gq? <Cmd>set formatprg?<CR>

" Unimpaired style mappings
nnoremap <expr> [a '<Cmd>' . v:count1 . 'prev<CR>'
nnoremap <expr> ]a '<Cmd>' . v:count1 . 'next<CR>'
nnoremap <expr> [b '<Cmd>' . v:count1 . 'bprev<CR>'
nnoremap <expr> ]b '<Cmd>' . v:count1 . 'bnext<CR>'
nnoremap <expr> [t '<Cmd>' . v:count1 . 'tprev<CR>'
nnoremap <expr> ]t '<Cmd>' . v:count1 . 'tnext<CR>'
nnoremap [A <Cmd>first<CR>
nnoremap ]A <Cmd>last<CR>
nnoremap [B <Cmd>bfirst<CR>
nnoremap ]B <Cmd>blast<CR>
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
nnoremap yoc <Cmd>setlocal cursorline!<Bar>set cul?<CR>
nnoremap yo<Bar> <Cmd>setlocal cursorcolumn!<Bar>set cuc?<CR>
nnoremap <expr> yod '<Cmd>' . (&diff ? 'diffoff' : 'diffthis') . '<CR>'
nnoremap yos <Cmd>setlocal spell!<Bar>set spell?<CR>
nnoremap yow <Cmd>setlocal wrap!<Bar>set wrap?<CR>

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

" Swap p and P in visual mode
vnoremap p P
vnoremap P p

" Use zS mapping from scriptease for :Inspect
nnoremap zS <Cmd>Inspect<CR>

augroup init
  autocmd!

  " Close preview and command windows with q
  autocmd BufWinEnter * if &previewwindow | nnoremap <buffer> q <C-W>q | endif
  autocmd CmdWinEnter * nnoremap <buffer> q <C-W>q

  " Highlight yanked text
  autocmd TextYankPost * lua vim.highlight.on_yank {higroup="Visual", timeout=150, on_visual=true}

  " Hide cursorline in insert mode and when the current window doesn't have
  " focus
  autocmd InsertEnter * setlocal nocursorline
  autocmd InsertLeave * setlocal cursorline
  autocmd WinLeave,FocusLost * if !&diff | setlocal nocursorline | endif
  autocmd InsertLeave,WinEnter,FocusGained * let &l:cursorline = mode() !=# 'i'

  " Create missing parent directories automatically
  autocmd BufNewFile * autocmd BufWritePre <buffer> ++once call mkdir(expand('%:h'), 'p')

  " Restore cursor position (except for git commits and rebases)
  autocmd BufRead * if &ft !~# 'commit\|rebase' | exec 'silent! normal! g`"' | endif

  " Don't show trailing spaces in insert mode
  autocmd InsertEnter * setlocal listchars-=trail:-
  autocmd InsertLeave * setlocal listchars+=trail:-

  " For some filetypes, completion based on syntax is better than nothing
  autocmd FileType cmake setlocal omnifunc=syntaxcomplete#Complete
augroup END

lua<<
_G.nvim = setmetatable({}, {
    __index = function(t, k)
        t[k] = assert(vim.api["nvim_" .. k], k)
        return t[k]
    end,
})
.
