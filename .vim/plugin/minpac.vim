if &compatible
    finish
endif

function! s:PackInit() abort
    silent! packadd minpac

    if !exists('g:loaded_minpac')
        let packpath = split(&packpath, ',')[0] . '/pack/minpac/opt'
        call mkdir(packpath, 'p')
        silent execute '!git clone https://github.com/k-takata/minpac' packpath . '/minpac'
        packadd minpac
    endif

    call minpac#init({'status_auto': v:true})

    call minpac#add('k-takata/minpac', {'type': 'opt'})

    " Colorscheme
    call minpac#add('gpanders/base16-vim', {'type': 'opt'})

    " Improved :Man command
    call minpac#add('gpanders/vim-man', {'type': 'opt'})

    " Improved :oldfiles command
    call minpac#add('gpanders/vim-oldfiles')

    " Evaluate code blocks in Markdown buffers
    call minpac#add('gpanders/vim-medieval')

    " Mappings to modify parenthesis and other surrounding tokens
    call minpac#add('tpope/vim-surround')

    " Repeat certain plugin actions
    call minpac#add('tpope/vim-repeat')

    " Mappings for commenting source code
    call minpac#add('tpope/vim-commentary')

    " Readline style bindings througout Vim
    call minpac#add('tpope/vim-rsi')

    " Vim debugging tools
    call minpac#add('tpope/vim-scriptease', {'type': 'opt'})

    " Git integration
    call minpac#add('tpope/vim-fugitive')

    " Better directory browser
    call minpac#add('justinmk/vim-dirvish')

    " Align lines to a character, e.g. =, ;, :, etc.
    call minpac#add('junegunn/vim-easy-align')

    " Show change signs for git files
    call minpac#add('mhinz/vim-signify')

    " Asynchronous linting
    call minpac#add('neomake/neomake')

    " Auto generate tags files
    call minpac#add('ludovicchabant/vim-gutentags')

    " Populate results of :ilist and :dlist in quickfix window
    call minpac#add('romainl/vim-qlist')

    " Better writing mode
    call minpac#add('junegunn/goyo.vim')

    " Language Server Client
    call minpac#add('natebosch/vim-lsc')

    " C / C++
    call minpac#add('bfrg/vim-cpp-modern')

    " xdc
    call minpac#add('amal-khailtash/vim-xdc-syntax')

    " fish
    call minpac#add('georgewitteman/vim-fish')

    " LaTeX
    call minpac#add('lervag/vimtex')

    " Markdown
    call minpac#add('gpanders/vim-markdown')

    " scdoc
    call minpac#add('gpanders/vim-scdoc')

    " zig
    call minpac#add('ziglang/zig.vim')

    " TOML
    call minpac#add('cespare/vim-toml')

    " PlantUML
    call minpac#add('aklt/plantuml-syntax')

    " Rust
    call minpac#add('rust-lang/rust.vim')

    " nim
    call minpac#add('zah/nim.vim')

    " Nix
    call minpac#add('LnL7/vim-nix')
endfunction

function! s:PackUpdate(...) abort
    call s:PackInit()
    call minpac#update(a:0 ? a:1 : '')
endfunction

function! s:PackClean(...) abort
    call s:PackInit()
    if a:0
        call minpac#clean(a:1)
    else
        call minpac#clean()
    endif
endfunction

augroup minpac
    autocmd!
    exe 'autocmd BufWritePost ' . expand('<sfile>') . ' source <afile>'
augroup END

command! -nargs=* PackUpdate call <SID>PackUpdate(<f-args>)
command! -nargs=* PackClean call <SID>PackClean(<f-args>)
