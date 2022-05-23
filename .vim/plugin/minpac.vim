function! s:PackInit() abort
    packadd minpac

    if !exists('g:loaded_minpac')
        let out = system('git clone https://github.com/k-takata/minpac ' .. $HOME .. '/.vim/pack/minpac/opt/minpac')
        if v:shell_error
            echo 'Error cloning minpac: ' .. out
            return
        endif
        packadd minpac
    endif

    call minpac#init()
    call minpac#add('k-takata/minpac', #{type: 'opt'})
    call minpac#add('tpope/vim-sleuth')
    call minpac#add('tpope/vim-repeat')
    call minpac#add('tpope/vim-commentary')
    call minpac#add('tpope/vim-surround')
endfunction

command! PackUpdate call s:PackInit() | call minpac#update()
command! PackClean  call s:PackInit() | call minpac#clean()
command! PackStatus packadd minpac | call minpac#status()
