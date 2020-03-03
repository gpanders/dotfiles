let s:save_cpo = &cpo
set cpo&vim

function! s:PackInit()
    silent! packadd minpac

    if exists('*minpac#init')
        call minpac#init()
        call minpac#add('k-takata/minpac', {'type': 'opt'})

        call minpac#add('gpanders/vim-man', {'type': 'opt'})
        call minpac#add('gpanders/vim-oldfiles')
        call minpac#add('gpanders/vim-medieval')

        " Tim Pope plugin suite
        call minpac#add('tpope/vim-fugitive')
        call minpac#add('tpope/vim-surround')
        call minpac#add('tpope/vim-repeat')
        call minpac#add('tpope/vim-commentary')
        call minpac#add('tpope/vim-dispatch')
        call minpac#add('tpope/vim-projectionist')
        call minpac#add('tpope/vim-rsi')
        call minpac#add('tpope/vim-eunuch')
        call minpac#add('tpope/vim-abolish')
        call minpac#add('tpope/vim-scriptease')
        call minpac#add('tpope/vim-obsession')

        " Better directory browser
        call minpac#add('justinmk/vim-dirvish')

        " Align lines to a character, e.g. =, ;, :, etc.
        call minpac#add('junegunn/vim-easy-align')

        " Show change signs in the gutter for git files
        call minpac#add('mhinz/vim-signify')

        " Auto generate tags files
        call minpac#add('ludovicchabant/vim-gutentags')

        " Populate results of :ilist and :dlist in quickfix window
        call minpac#add('romainl/vim-qlist')

        " Visualize undo tree
        call minpac#add('simnalamburt/vim-mundo')

        " Preview replacements when :substituting
        call minpac#add('markonm/traces.vim')

        " Language Packs {{{

        " xdc {{{
        call minpac#add('amal-khailtash/vim-xdc-syntax')
        " }}}

        " fish {{{
        call minpac#add('georgewitteman/vim-fish')
        " }}}

        " Python {{{
        " Provides compilers for flake8 and pylint
        call minpac#add('drgarcia1986/python-compilers.vim')
        " }}}

        " LaTeX {{{
        call minpac#add('lervag/vimtex')
        " }}}

        " Markdown {{{
        call minpac#add('tpope/vim-markdown')
        " }}}

        " scdoc {{{
        call minpac#add('gpanders/vim-scdoc')
        " }}}

        " }}}

        " Asynchronous linting
        call minpac#add('dense-analysis/ale')

        " Colorschemes
        call minpac#add('gpanders/base16-vim', {'type': 'opt'})
    endif
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
