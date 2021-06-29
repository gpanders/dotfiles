let g:loaded_telescope = 1

function! s:load(...)
    delcommand Telescope
    unlet g:loaded_telescope
    packadd telescope.nvim
    if exists('g:loaded_telescope')
        lua require('config.telescope')
    endif

    exec 'Telescope' join(a:000)
endfunction

command -nargs=* Telescope call s:load(<f-args>)
