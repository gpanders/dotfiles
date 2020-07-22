let s:save_cpo = &cpo
set cpo&vim

function! s:PackInit() abort
    silent! packadd minpac

    if !exists('*minpac#init')
        let packpath = split(&packpath, ',')[0]
        call mkdir(packpath . '/pack/minpac/opt', 'p')
        silent execute '!git clone https://github.com/k-takata/minpac ' . packpath . '/pack/minpac/opt/minpac'
        packadd minpac
    endif

    call minpac#init()
    call minpac#add('k-takata/minpac', {'type': 'opt'})

    " Colorscheme
    call minpac#add('gpanders/base16-vim', {'type': 'opt'})

    " Improved :Man command
    call minpac#add('gpanders/vim-man', {'type': 'opt'})

    " Improved :oldfiles command
    call minpac#add('gpanders/vim-oldfiles')

    " Evaluate code blocks in Markdown buffers
    call minpac#add('gpanders/vim-medieval')

    " Git wrapper in vim
    call minpac#add('tpope/vim-fugitive')

    " Mappings to modify parenthesis and other surrounding tokens
    call minpac#add('tpope/vim-surround')

    " Repeat certain plugin actions
    call minpac#add('tpope/vim-repeat')

    " Mappings for commenting source code
    call minpac#add('tpope/vim-commentary')

    " Asynchronous task runner
    call minpac#add('tpope/vim-dispatch')

    " Readline style bindings througout Vim
    call minpac#add('tpope/vim-rsi')

    " Unix commands in Vim
    call minpac#add('tpope/vim-eunuch')

    " Offers improved :substitute command and custom abbreviations
    call minpac#add('tpope/vim-abolish')

    " Vim debugging tools
    call minpac#add('tpope/vim-scriptease')

    " Session management
    call minpac#add('tpope/vim-obsession')

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

    " Automatically disable search highlighting
    call minpac#add('romainl/vim-cool')

    " Preview replacements when :substituting
    call minpac#add('markonm/traces.vim')

    " Better writing mode
    call minpac#add('junegunn/goyo.vim')

    " Auto completion
    call minpac#add('lifepillar/vim-mucomplete')

    " Language Server Client
    call minpac#add('natebosch/vim-lsc')

    " REPL
    call minpac#add('urbainvaes/vim-ripple')

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
endfunction

function! s:PackUpdate()
    call s:PackInit()
    if exists('*minpac#update')
        call minpac#update('', {'do': 'call minpac#status()'})
    endif
endfunction

function! s:PackClean()
    call s:PackInit()
    if exists('*minpac#clean')
        call minpac#clean()
    endif
endfunction

augroup minpac
    autocmd!
    exe 'autocmd BufWritePost ' . expand('<sfile>') . ' source <afile>'
augroup END

command! PackUpdate call <SID>PackUpdate()
command! PackClean call <SID>PackClean()

let &cpo = s:save_cpo
unlet s:save_cpo
