colorscheme nord

set breakindent
set cinoptions=l1,:0,g0,E-s,N-s,t0,(s,J1,j1
set colorcolumn=+1
set completeopt=menu,menuone,popup,noselect
set confirm
set cpoptions-=_
set cursorline
set cursorlineopt=number
set exrc
set foldlevelstart=99
set foldtext=
set ignorecase
set jumpoptions=view
set linebreak
set list
set listchars+=precedes:<,extends:>
set number
set pumheight=10
set scrolloff=2
set shada='100,<50,s10,:100,/100,h,r/tmp/,r/private/,rfugitive:,rzipfile:,rterm:
set sidescrolloff=5
set smartcase
set smoothscroll
set splitright
set statuscolumn=%s%=%l%{%&nu\|\|&rnu?'%#WinSeparator#│':''%}
set statusline=%{%v:lua.require'statusline'.statusline()%}
set tabline=%!v:lua.require'statusline'.tabline()
set tagcase=match
set title
set undofile
set updatetime=250
set wildignore+=*.pyc,__pycache__,.DS_Store,*~,#*#,*.o
set wildignorecase
set wildmode=longest:full,full
set winborder=rounded

if executable('rg')
  set grepprg=rg\ --vimgrep\ --smart-case
endif

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
nnoremap <Space>Y "+y$
nnoremap <Space>y "+y
xnoremap <Space>y "+y

" Search in visual selection with //
cnoremap <expr> / (getcmdtype() =~ '[/?]' && getcmdline() == '') ? "\<C-C>\<Esc>/\\%V" : '/'

" Use gK for keywordprg, since K gets mapped to 'hover' in LSP buffers
nnoremap gK K

" Format whole buffer with formatprg without changing cursor position
nnoremap gq<CR> mzgggqG`z
nnoremap gq? <Cmd>set formatprg? formatexpr?<CR>

" Unimpaired style mappings
nnoremap <expr> [e '<Cmd>.move --' . v:count1 . '<CR>'
nnoremap <expr> ]e '<Cmd>.move +' . v:count1 . '<CR>'
xnoremap <expr> [e ':move --' . v:count1 . '<CR>gv'
xnoremap <expr> ]e ':move +' . (v:count1 + line('''>') - line('''<')) . '<CR>gv'

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
inoremap <expr> <C-C> pumvisible() ? '<C-E>' : '<C-C>'
inoremap <expr> <CR> pumvisible() ? '<C-Y>' : '<CR>'

" Readline-style bindings in command and insert modes
inoremap <C-B> <Left>
inoremap <C-F> <Right>
cnoremap <C-A> <Home>
cnoremap <C-B> <Left>
cnoremap <expr> <C-F> getcmdpos() == (len(getcmdline()) + 1) ? '<C-F>' : '<Right>'
cnoremap <C-X><C-A> <C-A>
cnoremap <C-X><C-F> <C-F>

" Don't use fish for 'shell', but if it exists use it as the default shell in
" terminal buffers
nnoremap <expr> `<CR> '<Cmd>bo term' .. (executable('fish') ? ' fish' : '') .. '<CR>'
nnoremap <expr> `<Space> ':bo te '

" Use <Esc> to exit Terminal mode in terminal buffers
tnoremap <Esc> <C-\><C-N>

augroup init
  autocmd!

  " Close preview and command windows with q
  autocmd BufWinEnter * if &previewwindow | nnoremap <buffer> q <C-W>q | endif
  autocmd CmdWinEnter * nnoremap <buffer> q <C-W>q

  " Highlight yanked text
  autocmd TextYankPost * lua vim.hl.on_yank {higroup="Visual", timeout=150, on_visual=true}

  " Hide cursorline when the current window doesn't have focus
  autocmd WinLeave,FocusLost * if !&diff && !&cursorbind | setlocal nocursorline | endif
  autocmd WinEnter,FocusGained * setlocal cursorline

  " Create missing parent directories automatically
  autocmd BufNewFile * autocmd BufWritePre <buffer> ++once call mkdir(expand('%:h'), 'p')

  " Disable listchars in prompt buffers
  autocmd OptionSet buftype if &buftype ==# 'prompt' | setlocal nolist | endif

  " Don't show trailing spaces in insert mode
  autocmd InsertEnter * setlocal listchars-=trail:-
  autocmd InsertLeave * setlocal listchars<

  " For some filetypes, completion based on syntax is better than nothing
  autocmd FileType cmake setlocal omnifunc=syntaxcomplete#Complete

  " Automatically enter terminal mode when a :terminal opens
  autocmd TermOpen term://* startinsert
augroup END

lua<<
vim.loader.enable()

_G.nvim = vim.defaulttable(function(k)
    return vim.api[("nvim_%s"):format(k)]
end)
.
