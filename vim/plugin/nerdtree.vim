" NERDTree config
" NERDTree key binding
map <C-n> :NERDTreeToggle<CR>
nmap <leader>f :NERDTreeFind<CR>

" Open NERDTree when vim is opened on a directory
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif

"autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
